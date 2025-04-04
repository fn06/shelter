module History = struct
  type t = string

  let t = Repr.string
  let merge = Irmin.Merge.default (Repr.option t)
end

module Pass = Shelter.Make (History) (Shelter_passthrough)
module Shelter = Shelter.Make (Shelter_main.History) (Shelter_main)

let home = Unix.getenv "HOME"

let state_dir fs type' =
  let path = Eio.Path.(fs / home / ".cache/shelter" / type') in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 path;
  path

(* Command Line *)
open Cmdliner

let main =
  let run config =
    Eio_posix.run @@ fun env ->
    let dir = state_dir env#fs "shelter" in
    Shelter.main config env#fs env#clock env#process_mgr dir
  in
  let t = Term.(const run $ Shelter_main.config_term) in
  let man =
    [
      `P
        "Shelter is a shell session shim to help control uncertainty when \
         working from the terminal";
    ]
  in
  let doc = "Shelter: version-controlled shell sessions" in
  let info = Cmd.info ~man ~doc "main" in
  (Cmd.v info t, t, info)

let passthrough =
  let run config =
    Eio_posix.run @@ fun env ->
    let dir = state_dir env#fs "passthrough" in
    Pass.main config env#fs env#clock env#process_mgr dir
  in
  let t = Term.(const run $ Shelter_passthrough.config_term) in
  let info = Cmd.info "passthrough" in
  Cmd.v info t

let cmds =
  let cmd, term, info = main in
  let cmds = [ cmd; passthrough ] in
  Cmd.group ~default:term info cmds

let () =
  Fmt_tty.setup_std_outputs ();
  exit (Cmd.eval cmds)
