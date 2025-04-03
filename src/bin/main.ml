module History = struct
  type t = string

  let t = Repr.string
  let merge = Irmin.Merge.default (Repr.option t)
end

module Pass = Shelter.Make (History) (Shelter_passthrough)
module Shelter = Shelter.Make (Shelter_main.History) (Shelter_main)

let home = Unix.getenv "HOME"

let state_dir fs type' =
  let path = Eio.Path.(fs / home / ".cache/cshell" / type') in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 path;
  path

let () =
  Eio_posix.run @@ fun env ->
  Fmt_tty.setup_std_outputs ();
  match Sys.argv.(1) with
  | "shelter" ->
      let dir = state_dir env#fs "shelter" in
      Shelter.main env#fs env#clock env#process_mgr dir
  | _ | (exception Invalid_argument _) ->
      let dir = state_dir env#fs "passthrough" in
      Pass.main env#fs env#clock env#process_mgr dir
