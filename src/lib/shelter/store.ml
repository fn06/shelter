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
end = struct
  type dataset = string
  type snapshot = string

  let ( / ) a b = a ^ "/" ^ b
  let builds pool : dataset = pool / "builds"
  let build pool path : dataset = builds pool / path
  let snapshot ds = ds ^ "@snappy"
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

let unmount_dataset t (dataset : Datasets.dataset) =
  match Zfs.is_mounted t.zfs (dataset :> string) with
  | None -> ()
  | Some _ ->
      with_dataset t (dataset :> string) @@ fun d ->
      let _todo () = Zfs.unmount d in
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
  t

let snapshot t (snap : Datasets.snapshot) =
  let exists = Zfs.exists t.zfs (snap :> string) Zfs.Types.snapshot in
  if not exists then Zfs.snapshot t.zfs (snap :> string) true

let clone t (snap : Datasets.snapshot) (tgt : Datasets.dataset) =
  with_dataset ~typ:Zfs.Types.snapshot t (snap :> string) @@ fun src ->
  Zfs.clone src (tgt :> string)

let read_all fd =
  let buf = Buffer.create 128 in
  let bytes = Bytes.create 4096 in
  let rec loop () =
    match Unix.read fd bytes 0 4096 with
    | 0 | (exception End_of_file) -> Buffer.contents buf
    | n ->
        Buffer.add_bytes buf (Bytes.sub bytes 0 n);
        loop ()
  in
  loop ()

let diff t (data : Datasets.snapshot) (snap : Datasets.snapshot) =
  let data_fs =
    String.sub (data :> string) 0 (String.index (data :> string) '@')
  in
  let zh = Zfs.open_ t.zfs data_fs Zfs.Types.filesystem in
  let diff =
    let r, w = Unix.pipe ~cloexec:false () in
    try
      Zfs.show_diff zh ~from_:(data :> string) ~to_:(snap :> string) w;
      let f = read_all r in
      Unix.close r;
      f
    with e ->
      Unix.close r;
      raise e
  in
  Zfs.close zh;
  Diff.of_zfs diff

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
    fn ("/" ^ (ds :> string))

  let with_clone t ~src new_cid fn =
    let ds = Datasets.build t.pool (Cid.to_string src) in
    let tgt = Datasets.build t.pool (Cid.to_string new_cid) in
    let src_snap = Datasets.snapshot ds in
    let tgt_snap = Datasets.snapshot tgt in
    clone t src_snap tgt;
    let v = with_build t new_cid fn in
    snapshot t tgt_snap;
    let d = diff t src_snap tgt_snap in
    (v, d)
end
