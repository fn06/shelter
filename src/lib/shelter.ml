module History = History
module Engine = Engine
module Script = Script

let process_error e = Error (`Process e)
let shell_error e = Error (`Shell e)

module Make (H : History.S) (Engine : Engine.S with type entry = H.t) = struct
  module Store = Irmin_git_unix.FS.KV (H)

  let run config ~stdout fs clock proc store =
    let store = History.Store ((module Store), store) in
    let initial_ctx = Engine.init fs proc store in
    let rec loop store ctx exit_code =
      let prompt = Engine.prompt exit_code store in
      match LNoise.linenoise prompt with
      | None -> ()
      | Some input -> (
          let action = Engine.action_of_command input in
          match Engine.run config ~stdout fs clock proc (store, ctx) action with
          | Error (`Process (Eio.Process.Child_error exit_code)) ->
              Fmt.epr "%a\n%!" Eio.Process.pp_status exit_code;
              loop store ctx exit_code
          | Error (`Process (Eio.Process.Executable_not_found m)) ->
              Fmt.epr "shelter: excutable not found %s\n%!" m;
              loop store ctx (`Exited 127)
          | Error (`Shell e) ->
              Fmt.epr "shelter: %a\n%!" Engine.pp_error e;
              loop store ctx (`Exited 255)
          | Ok (store, ctx) -> loop store ctx (`Exited 0))
    in
    loop store initial_ctx (`Exited 0)

  let command_file_to_actions cf =
    Eio.Path.load cf |> String.split_on_char '\n'
    |> List.map Engine.action_of_command

  let main config ~stdout fs clock proc directory command_file =
    let conf = Irmin_git.config (Eio.Path.native_exn directory) in
    let repo = Store.Repo.v conf in
    let store = Store.main repo in
    match command_file with
    | Some file -> (
        let actions = command_file_to_actions file in
        let store = History.Store ((module Store), store) in
        let initial_ctx = Engine.init fs proc store in
        let folder (store, ctx, exit_code) action =
          if exit_code <> `Exited 0 then (store, ctx, exit_code)
          else
            match
              Engine.run config ~stdout fs clock proc (store, ctx) action
            with
            | Error (`Process (Eio.Process.Child_error exit_code)) ->
                Fmt.epr "%a\n%!" Eio.Process.pp_status exit_code;
                (store, ctx, exit_code)
            | Error (`Process (Eio.Process.Executable_not_found m)) ->
                Fmt.epr "shelter: excutable not found %s\n%!" m;
                (store, ctx, `Exited 127)
            | Error (`Shell e) ->
                Fmt.epr "shelter: %a\n%!" Engine.pp_error e;
                (store, ctx, `Exited 255)
            | Ok (store, ctx) -> (store, ctx, `Exited 0)
        in
        let _store, _ctx, exit_code =
          List.fold_left folder (store, initial_ctx, `Exited 0) actions
        in
        match exit_code with
        | `Exited 0 -> ()
        | `Exited n | `Signaled n ->
            Fmt.epr "%a\n%!" Eio.Process.pp_status exit_code;
            exit n)
    | None -> run config ~stdout fs clock proc store
end
