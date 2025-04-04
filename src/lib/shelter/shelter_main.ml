open Eio
module Store = Store
module H = Shelter.History

module History = struct
  type mode = Void.mode

  let mode_t =
    Repr.map Repr.string
      (function
        | "R" -> Void.R | "RW" -> Void.RW | _ -> failwith "Malformed Void.mode")
      (function Void.R -> "R" | Void.RW -> "RW")

  type t = {
    mode : mode;
    build : Store.Build.t;
    args : string list;
    time : int64;
    diff : Diff.t;
  }
  [@@deriving repr]

  let merge = Irmin.Merge.(default (Repr.option t))
end

type config = Config.t

let config_term = Config.cmdliner

type entry = History.t

type action =
  | Set_mode of History.mode
  | Set_session of string
  | Exec of string list
  | Info
  | Undo
  | Fork of string
  | Replay of string
  | Unknown of string list
  | History
[@@deriving repr]

let split_and_remove_empty s =
  String.split_on_char ' ' s |> List.filter (fun v -> not (String.equal "" v))

let action = action_t

let shelter_action = function
  | "mode" :: [ "r" ] -> Set_mode R
  | "mode" :: [ "rw" ] -> Set_mode R
  | "session" :: [ m ] -> Set_session m
  | "fork" :: [ m ] -> Fork m
  | "replay" :: [ onto ] -> Replay onto
  | [ "info" ] -> Info
  | [ "undo" ] -> Undo
  | [ "history" ] -> History
  | other -> Unknown other

let action_of_command cmd =
  match split_and_remove_empty cmd with
  | "@" :: rest -> shelter_action rest
  | args -> Exec args

let () = Fmt.set_style_renderer Format.str_formatter `Ansi_tty
let history_key = [ "history" ]
let key clock = history_key @ [ string_of_float @@ Eio.Time.now clock ]

let list (H.Store ((module S), store) : entry H.t) =
  match S.list store history_key with
  | [] -> []
  | xs ->
      let rec loop acc = function
        | (s, `Contents (v, _meta)) :: next -> loop ((s, v) :: acc) next
        | _ :: next -> loop acc next
        | [] -> List.rev acc
      in
      loop [] (List.map (fun (v, tree) -> (v, S.Tree.to_concrete tree)) xs)
      |> List.stable_sort (fun (s1, _) (s2, _) ->
             Float.compare (Float.of_string s1) (Float.of_string s2))
      |> List.rev

let with_latest ~default s f =
  match list s with [] -> default () | hd :: _ -> f hd

let text c = Fmt.(styled (`Fg c) string)

let sessions (H.Store ((module S), store) : entry H.t) =
  S.Branch.list (S.repo store)

let commit ~message clock (H.Store ((module S), store) : entry H.t) v =
  let info () = S.Info.v ~message (Eio.Time.now clock |> Int64.of_float) in
  S.set_exn ~info store (key clock) v

let which_branch ((H.Store ((module S), session) : entry H.t) as s) =
  let branches = sessions s in
  let repo = S.repo session in
  let heads = List.map (fun b -> (S.Branch.find repo b, b)) branches in
  let head = S.Head.find session in
  List.assoc_opt head heads

(* Reset the head of the current session by one commit *)
let reset_hard ((H.Store ((module S), session) : entry H.t) as s) =
  match
    List.filter_map (S.Commit.of_hash (S.repo session))
    @@ S.Commit.parents (S.Head.get session)
  with
  | [] -> s
  | p :: _ ->
      S.Head.set session p;
      s

(* Fork a new session from an existing one *)
let fork (H.Store ((module S), session) : entry H.t) new_branch =
  let repo = S.repo session in
  match (S.Head.find session, S.Branch.find repo new_branch) with
  | _, Some _ ->
      Error (new_branch ^ " already exists, try @ session " ^ new_branch)
  | None, _ -> Error "Current branch needs at least one commit"
  | Some commit, None ->
      let new_store = S.of_branch (S.repo session) new_branch in
      S.Branch.set repo new_branch commit;
      let store = H.Store ((module S), new_store) in
      Ok store

(* Fork a new session from an existing one *)
let display_history (H.Store ((module S), session) : entry H.t) =
  let history = S.history ~depth:max_int session in
  let content c =
    H.Store ((module S), S.of_commit c) |> list |> List.hd |> snd
  in
  let pp_diff fmt d =
    if d = [] then () else Fmt.pf fmt "\n  %a" (Repr.pp Diff.t) d
  in
  let pp_entry fmt (e : entry) =
    Fmt.pf fmt "%-10s %s%a"
      Fmt.(str "%a" (styled (`Fg `Yellow) uint64_ns_span) e.time)
      (String.concat " " e.args) pp_diff e.diff
  in
  let linearize =
    S.History.fold_vertex (fun c v -> content c :: v) history [] |> List.rev
  in
  List.iter (fun c -> Fmt.pr "%a\n%!" pp_entry c) linearize

let prompt status ((H.Store ((module S), _session) : entry H.t) as store) =
  let sesh = Option.value ~default:"main" (which_branch store) in
  let prompt () =
    Fmt.(styled (`Fg `Yellow) string) Format.str_formatter "shelter> ";
    Format.flush_str_formatter ()
  in
  let pp_sesh fmt sesh = Fmt.pf fmt "[%a]" (text `Green) sesh in
  let pp_status fmt = function
    | `Exited 0 -> Fmt.nop fmt ()
    | `Exited n -> Fmt.pf fmt "%a " (text `Red) (string_of_int n)
    | _ -> Fmt.nop fmt ()
  in
  let prompt_entry (_, (e : entry)) =
    Fmt.pf Format.str_formatter "%a%a%a : { mode: %a }> " pp_status status
      (text `Yellow) "shelter" pp_sesh sesh (text `Red)
      (if e.mode = R then "r" else "rw");
    Format.flush_str_formatter ()
  in
  with_latest store ~default:prompt prompt_entry

type ctx = Store.t

let init fs proc s =
  let store = Store.init fs proc "shelter" in
  List.iter
    (fun (_, { History.args; _ }) ->
      LNoise.history_add (String.concat " " args) |> ignore)
    (list s);
  store

let run (_config : config) _fs clock _proc
    (((H.Store ((module S), store) : entry H.t) as s), ctx) = function
  | Set_mode mode ->
      with_latest ~default:(fun _ -> Ok (s, ctx)) s @@ fun (_, entry) ->
      commit ~message:"mode change" clock s { entry with mode };
      Ok (s, ctx)
  | Set_session m ->
      with_latest ~default:(fun _ -> Ok (s, ctx)) s @@ fun (_, entry) ->
      let new_store = S.of_branch (S.repo store) m in
      let new_full_store = H.Store ((module S), new_store) in
      commit ~message:"new session" clock new_full_store entry;
      Ok (new_full_store, ctx)
  | Unknown args ->
      Fmt.epr "%a: %s\n%!" (text `Red) "Unknown Shelter Action"
        (String.concat " " args);
      Ok (s, ctx)
  | Info ->
      let sessions = sessions s in
      let sesh = Option.value ~default:"main" (which_branch s) in
      let history = S.history store in
      let pp_commit fmt (hash, msg) =
        Fmt.pf fmt "[%a]: %s" (text `Yellow) hash msg
      in
      let commits =
        S.History.fold_vertex
          (fun commit acc ->
            let info = S.Commit.info commit |> S.Info.message in
            let hash = S.Commit.hash commit |> Repr.to_string S.Hash.t in
            (String.sub hash 0 7, info) :: acc)
          history []
      in
      let latest =
        with_latest
          ~default:(fun () -> None)
          s
          (fun (_, e) -> Some (Repr.to_string Store.Build.t e.build))
      in
      Fmt.pr "Sessions: %a\nCurrent: %a\nHash: %a\nCommits:@.  %a\n%!"
        Fmt.(list ~sep:(Fmt.any ", ") string)
        sessions (text `Green) sesh
        Fmt.(option string)
        latest
        Fmt.(vbox ~indent:2 @@ list pp_commit)
        commits;
      Ok (s, ctx)
  | Exec [] -> Ok (s, ctx)
  | Undo -> Ok (reset_hard s, ctx)
  | Fork new_branch -> (
      match fork s new_branch with
      | Error err ->
          Fmt.pr "[fork]: %a\n%!" (text `Red) err;
          Ok (s, ctx)
      | Ok store -> Ok (store, ctx))
  | Replay _ -> Ok (s, ctx)
  | History ->
      display_history s;
      Ok (s, ctx)
  | Exec command -> (
      let entry =
        with_latest
          ~default:(fun () ->
            History.
              {
                mode = Void.RW;
                build = Store.Build.Image "alpine";
                args = command;
                time = 0L;
                diff = [];
              })
          s
        @@ fun (_, e) -> e
      in
      let build =
        match entry.build with
        | Store.Build.Image img -> Store.fetch ctx img
        | Store.Build.Build cid -> cid
      in
      let hash_entry = { entry with build = Build build; args = command } in
      let new_cid = Store.cid (Repr.to_string History.t hash_entry) in
      let with_rootfs fn =
        if entry.mode = R then (Store.Run.with_build ctx build fn, [])
        else Store.Run.with_clone ctx ~src:build new_cid fn
      in
      try
        let new_entry, diff =
          with_rootfs @@ fun rootfs ->
          let void =
            Void.empty
            |> Void.rootfs ~mode:entry.mode rootfs
            |> Void.exec [ "/bin/ash"; "-c"; String.concat " " command ]
          in
          Switch.run @@ fun sw ->
          let start = Mtime_clock.now () in
          let proc = Void.spawn ~sw void in
          let res =
            Void.exit_status proc |> Eio.Promise.await |> Void.to_eio_status
          in
          let stop = Mtime_clock.now () in
          let span = Mtime.span start stop in
          let time = Mtime.Span.to_uint64_ns span in
          (* Add command to history regardless of exit status *)
          let _ : (unit, string) result =
            LNoise.history_add (String.concat " " command)
          in
          if res = `Exited 0 then
            if entry.mode = RW then
              Ok { hash_entry with build = Build new_cid; time }
            else Ok hash_entry
          else Error (Eio.Process.Child_error res)
        in
        match new_entry with
        | Error e -> Error e
        | Ok entry ->
            (* Set diff *)
            let entry = { entry with diff } in
            (* Commit if RW *)
            if entry.mode = RW then
              commit
                ~message:("exec " ^ String.concat " " command)
                clock s entry;
            Ok (s, ctx)
      with Eio.Exn.Io (Eio.Process.E e, _) -> Error e)
