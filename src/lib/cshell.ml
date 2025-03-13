module History = History
module Engine = Engine

module Make (H : History.S) (Engine : Engine.S with type entry = H.t) = struct
  module Store = Irmin_fs_unix.KV.Make (H)

  let run clock proc store =
    let store = History.Store ((module Store), store) in
    Engine.init store;
    let rec loop store exit_code =
      let prompt = Engine.prompt exit_code store in
      match LNoise.linenoise prompt with
      | None -> ()
      | Some input -> (
          let action = Engine.action_of_command input in
          match Engine.run clock proc store action with
          | Error (Eio.Process.Child_error exit_code) ->
              Fmt.epr "%a\n%!" Eio.Process.pp_status exit_code;
              loop store exit_code
          | Error (Eio.Process.Executable_not_found m) ->
              Fmt.epr "cshell: excutable not found %s\n%!" m;
              loop store (`Exited 127)
          | Ok store -> loop store (`Exited 0))
    in
    loop store (`Exited 0)

  let main clock proc directory =
    Irmin_fs.run directory @@ fun () ->
    let conf = Irmin_fs.config (Eio.Path.native_exn directory) in
    let repo = Store.Repo.v conf in
    let store = Store.main repo in
    run clock proc store
end
