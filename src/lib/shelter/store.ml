(* A store a bit like OBuilder's but a little simplified
   for our purposes *)
module Build = struct
  type cid = Cid.t

  let cid_of_string s =
    match Cid.of_string s with
    | Ok v -> v
    | Error (`Msg m) -> failwith m
    | Error (`Unsupported _) -> failwith "unsupported cid"

  let cid_t = Repr.map Repr.string cid_of_string Cid.to_string

  type t = Image of string | Build of cid [@@deriving repr]
end

type path = string list

type t = {
  fs : Eio.Fs.dir_ty Eio.Path.t;
  proc : Eio_unix.Process.mgr_ty Eio_unix.Process.mgr;
  zfs : Zfs.Handle.t;
  pool : string;
}

module Datasets : sig
  type dataset = private string
  type snapshot = private string

  val builds : string -> dataset
  val build : string -> string -> dataset
  val snapshot : dataset -> snapshot
  val tools : string -> dataset
  val tool : string -> string -> dataset
end = struct
  type dataset = string
  type snapshot = string

  let ( / ) a b = a ^ "/" ^ b
  let builds pool : dataset = pool / "builds"
  let build pool path : dataset = builds pool / path
  let snapshot ds = ds ^ "@snappy"
  let tools pool : dataset = pool / "tools"
  let tool pool path : dataset = tools pool / path
end

let with_dataset ?(typ = Zfs.Types.filesystem) t dataset f =
  let exists = Zfs.exists t.zfs (dataset :> string) typ in
  if not exists then Zfs.create t.zfs dataset typ;
  let dataset = Zfs.open_ t.zfs dataset typ in
  Fun.protect ~finally:(fun () -> Zfs.close dataset) (fun () -> f dataset)

let mount_dataset ?(typ = Zfs.Types.dataset) t (dataset : Datasets.dataset) =
  match Zfs.is_mounted t.zfs (dataset :> string) with
  | Some _ -> ()
  | None -> with_dataset ~typ t (dataset :> string) @@ fun d -> Zfs.mount d

let mount_snapshot ?(typ = Zfs.Types.snapshot) t (dataset : Datasets.snapshot) =
  match Zfs.is_mounted t.zfs (dataset :> string) with
  | Some _ -> ()
  | None -> with_dataset ~typ t (dataset :> string) @@ fun d -> Zfs.mount d

let unmount_dataset t (dataset : Datasets.dataset) =
  match Zfs.is_mounted t.zfs (dataset :> string) with
  | None -> ()
  | Some _ ->
      with_dataset t (dataset :> string) @@ fun d ->
      let () = Zfs.unmount d in
      ()

let create_dataset t (dataset : Datasets.dataset) =
  with_dataset t (dataset :> string) (fun _ -> ())

let create_and_mount t (dataset : Datasets.dataset) =
  create_dataset t dataset;
  mount_dataset t dataset

let init fs proc pool =
  let zfs = Zfs.init () in
  Zfs.debug zfs true;
  let t =
    {
      fs :> Eio.Fs.dir_ty Eio.Path.t;
      proc :> Eio_unix.Process.mgr_ty Eio_unix.Process.mgr;
      zfs;
      pool;
    }
  in
  create_and_mount t (Datasets.builds t.pool);
  create_and_mount t (Datasets.tools t.pool);
  t

let snapshot t (snap : Datasets.snapshot) =
  let exists = Zfs.exists t.zfs (snap :> string) Zfs.Types.snapshot in
  if not exists then Zfs.snapshot t.zfs (snap :> string) true

let clone t (snap : Datasets.snapshot) (tgt : Datasets.dataset) =
  with_dataset ~typ:Zfs.Types.snapshot t (snap :> string) @@ fun src ->
  Zfs.clone src (tgt :> string)

let read_all fd =
  let buf = Buffer.create 10_000 in
  let bytes = Bytes.create 10_000 in
  let rec loop () =
    match Unix.read fd bytes 0 4096 with
    | 0 | (exception End_of_file) -> Buffer.contents buf
    | n ->
        Buffer.add_bytes buf (Bytes.sub bytes 0 n);
        loop ()
  in
  loop ()

let cid s =
  let hash =
    Multihash_digestif.of_cstruct `Sha2_256 (Cstruct.of_string s)
    |> Result.get_ok
  in
  Cid.v ~version:`Cidv1 ~base:`Base32 ~codec:`Raw ~hash

let not_empty s = not String.(equal empty s)

let get_uid_gid ~username rootfs =
  let passwd =
    Eio.Path.load Eio.Path.(rootfs / "etc" / "passwd")
    |> String.split_on_char '\n'
    |> List.map (String.split_on_char ':')
    |> List.map (List.filter not_empty)
  in
  List.find_map
    (function
      | user :: _ :: uid :: gid :: _ when String.equal user username ->
          Some (int_of_string uid, int_of_string gid)
      | _ -> None)
    passwd

let fetch t image =
  let cid = cid image in
  let cids = cid |> Cid.to_string in
  let dataset = Datasets.build t.pool cids in
  let dir = Eio.Path.(t.fs / ("/" ^ (Datasets.build t.pool cids :> string))) in
  create_and_mount t dataset;
  let _dir : string = Fetch.get_image ~dir ~proc:t.proc image in
  snapshot t (Datasets.snapshot dataset);
  let username = Fetch.get_user t.proc image in
  ( cid,
    Fetch.get_env t.proc image,
    get_uid_gid ~username Eio.Path.(dir / "rootfs") )

module Run = struct
  let with_build t cid fn =
    let ds = Datasets.build t.pool (Cid.to_string cid) in
    Fun.protect ~finally:(fun () -> unmount_dataset t ds) @@ fun () ->
    mount_dataset t ds;
    fn (`Build ("/" ^ (ds :> string)))

  let with_tool t cid fn =
    let ds = Datasets.tool t.pool (Cid.to_string cid) in
    Fun.protect ~finally:(fun () -> unmount_dataset t ds) @@ fun () ->
    mount_dataset t ds;
    fn ("/" ^ (ds :> string))

  let diff t (data : Datasets.snapshot) (snap : Datasets.snapshot) output =
    let data_fs =
      String.sub (data :> string) 0 (String.index (data :> string) '@')
    in
    let snap_fs =
      String.sub (snap :> string) 0 (String.index (snap :> string) '@')
    in
    if Option.is_none (Zfs.is_mounted t.zfs data_fs) then
      with_dataset t data_fs Zfs.mount;
    if Option.is_none (Zfs.is_mounted t.zfs snap_fs) then
      with_dataset t snap_fs Zfs.mount;
    with_dataset ~typ:Zfs.Types.filesystem t data_fs @@ fun zh ->
    let () =
      try
        Eio.Path.with_open_out ~create:(`If_missing 0o644) output
        @@ fun flow_fd ->
        let eio_fd = Eio_unix.Resource.fd_opt flow_fd in
        Eio_unix.Fd.use_exn_opt "zfs-diff" eio_fd @@ function
        | None -> Fmt.failwith "Output needs to have an FD"
        | Some fd ->
            Zfs.show_diff zh ~from_:(data :> string) ~to_:(snap :> string) fd
      with Unix.Unix_error (Unix.EBADF, _, _) -> ()
    in
    let diff = Eio.Path.load output in
    Diff.of_zfs diff

  let with_clone t ~src new_cid output fn =
    let ds = Datasets.build t.pool (Cid.to_string src) in
    let tgt = Datasets.build t.pool (Cid.to_string new_cid) in
    let src_snap = Datasets.snapshot ds in
    let tgt_snap = Datasets.snapshot tgt in
    if Zfs.exists t.zfs (tgt :> string) Zfs.Types.dataset then
      (fn (`Exists ("/" ^ (tgt :> string))), diff t src_snap tgt_snap output)
    else (
      clone t src_snap tgt;
      let v = with_build t new_cid fn in
      snapshot t tgt_snap;
      let d = diff t src_snap tgt_snap output in
      (v, d))
end
