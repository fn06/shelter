open Eio

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

type mode = R | RW [@@deriving repr]

module History = struct
  type t = { mode : mode; build : Build.t; args : string list }
  [@@deriving repr]

  let merge = Irmin.Merge.(default (Repr.option t))
end

type entry = History.t

type action =
  | Set_mode of mode
  | Set_session of string
  | Exec of string list
  | Info
  | Unknown of string list
[@@deriving repr]

let split_and_remove_empty s =
  String.split_on_char ' ' s |> List.filter (fun v -> not (String.equal "" v))

let action = action_t

let shelter_action = function
  | "set" :: "mode" :: [ "r" ] -> Set_mode R
  | "set" :: "mode" :: [ "rw" ] -> Set_mode R
  | "session" :: [ m ] -> Set_session m
  | [ "info" ] -> Info
  | other -> Unknown other

let action_of_command cmd =
  match split_and_remove_empty cmd with
  | "@" :: rest -> shelter_action rest
  | args -> Exec args

let () = Fmt.set_style_renderer Format.str_formatter `Ansi_tty
let history_key = [ "history" ]
let key clock = history_key @ [ string_of_float @@ Eio.Time.now clock ]

let list (Cshell.History.Store ((module S), store) : entry Cshell.History.t) =
  match S.list store history_key with
  | [] -> []
  | xs ->
      let rec loop acc = function
        | (s, `Contents (v, _meta)) :: next -> loop ((s, v) :: acc) next
        | _ :: next -> loop acc next
        | [] -> List.rev acc
      in
      loop [] (List.map (fun (v, tree) -> (v, S.Tree.to_concrete tree)) xs)
      |> List.stable_sort (fun (s1, _) (s2, _) -> String.compare s1 s2)
      |> List.rev

let with_latest ~default s f =
  match list s with [] -> default () | hd :: _ -> f hd

let text c = Fmt.(styled (`Fg c) string)

let sessions (Cshell.History.Store ((module S), store) : entry Cshell.History.t)
    =
  S.Branch.list (S.repo store)

let commit ~message clock
    (Cshell.History.Store ((module S), store) : entry Cshell.History.t) v =
  let info () = S.Info.v ~message (Eio.Time.now clock |> Int64.of_float) in
  S.set_exn ~info store (key clock) v

let which_branch
    ((Cshell.History.Store ((module S), session) : entry Cshell.History.t) as s)
    =
  let branches = sessions s in
  let repo = S.repo session in
  let heads = List.map (fun b -> (S.Branch.find repo b, b)) branches in
  let head = S.Head.find session in
  List.assoc_opt head heads

let prompt status
    ((Cshell.History.Store ((module S), _session) : entry Cshell.History.t) as
     store) =
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

let init s =
  List.iter
    (fun (_, { History.args; _ }) ->
      LNoise.history_add (String.concat " " args) |> ignore)
    (list s)

let run clock proc
    ((Cshell.History.Store ((module S), store) : entry Cshell.History.t) as s) =
  function
  | Set_mode mode ->
      with_latest ~default:(fun _ -> Ok s) s @@ fun (_, entry) ->
      commit ~message:"mode change" clock s { entry with mode };
      Ok s
  | Set_session m ->
      with_latest ~default:(fun _ -> Ok s) s @@ fun (_, entry) ->
      let new_store = S.of_branch (S.repo store) m in
      let new_full_store = Cshell.History.Store ((module S), new_store) in
      commit ~message:"new session" clock new_full_store entry;
      Ok new_full_store
  | Unknown args ->
      Fmt.epr "%a: %s\n%!" (text `Red) "Unknown Shelter Action"
        (String.concat " " args);
      Ok s
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
            ((String.sub hash 0 7), info) :: acc)
          history []
      in
      Fmt.pr "Sessions: %a\nCurrent: %a\nCommits:@.  %a\n%!"
        Fmt.(list ~sep:(Fmt.any ", ") string)
        sessions (text `Green) sesh
        Fmt.(vbox ~indent:2 @@ list pp_commit)
        commits;
      Ok s
  | Exec [] -> Ok s
  | Exec command -> (
      Switch.run @@ fun sw ->
      try
        let proc = Eio.Process.spawn ~sw proc [ "bash"; "-c"; String.concat " " command ] in
        let res = Eio.Process.await proc in
        if res = `Exited 0 then (
          let entry =
            History.{ mode = RW; build = Image "TODO"; args = command }
          in
          commit ~message:("exec " ^ (String.concat " " command)) clock s entry;
          let _ : (unit, string) result =
            LNoise.history_add (String.concat " " command)
          in
          Ok s)
        else Error (Eio.Process.Child_error res)
      with Eio.Exn.Io (Eio.Process.E e, _) -> Error e)
