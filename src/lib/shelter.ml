module History = History
module Engine = Engine

module Make (H : History.S) (Engine : Engine.S with type entry = H.t) = struct
  module Store = Irmin_fs_unix.KV.Make (H)

  let run config fs clock proc store =
    let store = History.Store ((module Store), store) in
    let initial_ctx = Engine.init fs proc store in
    let rec loop store ctx exit_code =
      let prompt = Engine.prompt exit_code store in
      match LNoise.linenoise prompt with
      | None -> ()
      | Some input -> (
          let action = Engine.action_of_command input in
          match Engine.run config fs clock proc (store, ctx) action with
          | Error (Eio.Process.Child_error exit_code) ->
              Fmt.epr "%a\n%!" Eio.Process.pp_status exit_code;
              loop store ctx exit_code
          | Error (Eio.Process.Executable_not_found m) ->
              Fmt.epr "cshell: excutable not found %s\n%!" m;
              loop store ctx (`Exited 127)
          | Ok (store, ctx) -> loop store ctx (`Exited 0))
    in
    loop store initial_ctx (`Exited 0)

  let main config fs clock proc directory =
    Irmin_fs.run directory @@ fun () ->
    let conf = Irmin_fs.config (Eio.Path.native_exn directory) in
    let repo = Store.Repo.v conf in
    let store = Store.main repo in
    run config fs clock proc store
end
