module History = History
module Engine = Engine
module Script = Script

let process_error e = Error (`Process e)
let shell_error e = Error (`Shell e)

module Make (H : Irmin.Contents.S) (Engine : Engine.S with type contents = H.t) =
struct
  module Store = Irmin_git_unix.FS.KV (H)

  let run config env store =
    let store = History.Store ((module Store), store) in
    let initial_store = Engine.init env#fs env#process_mgr store in
    let rec loop store exit_code =
      try
        let prompt = Engine.prompt exit_code store in
        match LNoise.linenoise prompt with
        | None -> ()
        | Some input -> (
            let action = Engine.action_of_command input in
            match Engine.run config env store action with
            | Error (`Process (Eio.Process.Child_error exit_code)) ->
                Fmt.epr "%a\n%!" Eio.Process.pp_status exit_code;
                loop store exit_code
            | Error (`Process (Eio.Process.Executable_not_found m)) ->
                Fmt.epr "shelter: excutable not found %s\n%!" m;
                loop store (`Exited 127)
            | Error (`Shell e) ->
                Fmt.epr "shelter: %a\n%!" Engine.pp_error e;
                loop store (`Exited 255)
            | Ok store -> loop store (`Exited 0))
      with Sys.Break -> loop store (`Exited 130)
    in
    loop initial_store (`Exited 0)

  let command_file_to_actions cf =
    Eio.Path.load cf |> String.split_on_char '\n'
    |> List.map Engine.action_of_command

  let main config env directory command_file =
    let conf = Irmin_git.config (Eio.Path.native_exn directory) in
    let repo = Store.Repo.v conf in
    let store = Store.main repo in
    match command_file with
    | Some file -> (
        let actions = command_file_to_actions file in
        let store = History.Store ((module Store), store) in
        let initial_store = Engine.init env#fs env#process_mgr store in
        let folder (store, exit_code) action =
          if exit_code <> `Exited 0 then (store, exit_code)
          else
            match Engine.run config env store action with
            | Error (`Process (Eio.Process.Child_error exit_code)) ->
                Fmt.epr "%a\n%!" Eio.Process.pp_status exit_code;
                (store, exit_code)
            | Error (`Process (Eio.Process.Executable_not_found m)) ->
                Fmt.epr "shelter: excutable not found %s\n%!" m;
                (store, `Exited 127)
            | Error (`Shell e) ->
                Fmt.epr "shelter: %a\n%!" Engine.pp_error e;
                (store, `Exited 255)
            | Ok store -> (store, `Exited 0)
        in
        let _store, exit_code =
          List.fold_left folder (initial_store, `Exited 0) actions
        in
        match exit_code with
        | `Exited 0 -> ()
        | `Exited n | `Signaled n ->
            Fmt.epr "%a\n%!" Eio.Process.pp_status exit_code;
            exit n)
    | None -> run config env store
end
