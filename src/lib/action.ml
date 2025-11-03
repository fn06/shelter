type session = { name : string; image : string option } [@@deriving repr]

type t =
  (* Change modes *)
  | Set_mode of History.mode
  (* Fork a new branch from an existing one,
     or switch to a branch if it exists *)
  | Session of session (* Run a command *)
  | Exec of string list
  (* Run a command but don't update the state *)
  | Check of string list
  (* Undo the last command *)
  | Undo
  (* Replay the current branch onto another *)
  | Replay of string
  (* Merge one branch into another *)
  | Merge of string
  (* Display info *)
  | Info of [ `Current | `History ]
  (* Error state *)
  | Unknown of string list
[@@deriving repr]

let pp fmt = function
  | Set_mode R -> Fmt.pf fmt "%@ mode r"
  | Set_mode RW -> Fmt.pf fmt "%@ mode rw"
  | Session { name; image } ->
      Fmt.pf fmt "%@ session %s %a" name Fmt.(option string) image
  | Undo -> Fmt.string fmt "%@ undo"
  | Replay onto -> Fmt.pf fmt "%@ replay %s" onto
  | Merge into -> Fmt.pf fmt "%@ merge %s" into
  | Info `Current -> Fmt.pf fmt "%@ info"
  | Info `History -> Fmt.pf fmt "%@ history"
  | Exec exec -> Fmt.(list ~sep:(Fmt.any " ") string) fmt exec
  | Check exec -> Fmt.(list ~sep:(Fmt.any " ") string) fmt exec
  | Unknown u -> Fmt.pf fmt "unknown: %a" Fmt.(list ~sep:(Fmt.any " ") string) u

let t = Repr.like ~pp t

let split_and_remove_empty s =
  String.split_on_char ' ' s |> List.filter (fun s -> not (String.equal s ""))

open Cmdliner

let session_name =
  let doc = "Name of the session." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)

let branch_name =
  let doc = "Name of a branch." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"BRANCH" ~doc)

let image =
  let doc = "Base image name." in
  Arg.(value & opt (some string) None & info [ "image" ] ~docv:"IMAGE" ~doc)

let session =
  let make_session name image = Session { name; image } in
  let session_term = Term.(const make_session $ session_name $ image) in
  let session_info =
    let doc = "Manage a session" in
    Cmd.info "session" ~doc
  in
  Cmd.v session_info session_term

let replay =
  let make_replay branch = Replay branch in
  let term = Term.(const make_replay $ branch_name) in
  let info =
    let doc = "Replay a branch on top of another" in
    Cmd.info "replay" ~doc
  in
  Cmd.v info term

let merge =
  let make_merge branch = Merge branch in
  let term = Term.(const make_merge $ branch_name) in
  let info =
    let doc = "Merge a branch into your current branch" in
    Cmd.info "merge" ~doc
  in
  Cmd.v info term

let undo =
  let make_undo = Undo in
  let term = Term.(const make_undo) in
  let info =
    let doc = "Undo your last action." in
    Cmd.info "undo" ~doc
  in
  Cmd.v info term

let info =
  let make_info = Info `Current in
  let term = Term.(const make_info) in
  let info =
    let doc = "Information about your current session." in
    Cmd.info "info" ~doc
  in
  Cmd.v info term

let history =
  let make_history = Info `History in
  let term = Term.(const make_history) in
  let info =
    let doc = "See the history of your current session." in
    Cmd.info "history" ~doc
  in
  Cmd.v info term

let all_commands =
  let meta = Cmd.info "@" ~doc:"Meta-commands for Shelter" in
  let default =
    Term.(ret (const (`Help (`Auto, None))))
    (* show help *)
  in
  Cmd.group meta ~default [ session; replay; merge; undo; info; history ]

let of_string s =
  let args = split_and_remove_empty s in
  match args with
  | "@" :: _ as cmd -> (
      let t = Cmd.eval_value ~argv:(Array.of_list cmd) all_commands in
      match t with
      | Ok (`Ok v) -> Some v
      | Ok `Version | Ok `Help | Error _ -> None)
  | args -> Some (Exec args)
