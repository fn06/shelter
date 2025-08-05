let home = Unix.getenv "HOME"

let state_dir fs type' =
  let path = Eio.Path.(fs / home / ".cache/shelter" / type') in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 path;
  path

module Eventloop = struct
  let run fn =
    Eio_posix.run @@ fun env ->
    Lwt_eio.with_event_loop ~clock:env#clock @@ fun _token -> fn env
end

(* Command Line *)
open Cmdliner

let cmd_file =
  let doc = "Path to a file containing a series of commands." in
  Arg.(
    value
    & opt (some file) None
    & info [ "f"; "file" ] ~docv:"COMMAND_FILE" ~doc)

let main =
  let run config cmd_file =
    Eventloop.run @@ fun env ->
    let cmd_file = Option.map (Eio.Path.( / ) env#fs) cmd_file in
    let dir = state_dir env#fs "shelter" in
    Shelter.main config (env :> _ Shelter.env) dir cmd_file
  in
  let t = Term.(const run $ Shelter.Engine.config_term $ cmd_file) in
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

let cmds =
  let cmd, term, info = main in
  let cmds = [ cmd ] @ Shelter.Engine.cmds in
  Cmd.group ~default:term info cmds

let () =
  Fmt_tty.setup_std_outputs ();
  exit (Cmd.eval cmds)
