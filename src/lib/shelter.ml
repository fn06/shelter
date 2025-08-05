module History = History
module Engine = Engine

type 'a env =
  < clock : [> float Eio.Time.clock_ty ] Eio.Resource.t
  ; fs : Eio.Fs.dir_ty Eio.Path.t
  ; net : [> [> `Generic | `Unix ] Eio.Net.ty ] Eio.Resource.t
  ; process_mgr : [> [> `Generic ] Eio.Process.mgr_ty ] Eio.Resource.t
  ; stdout : [> Eio.Flow.sink_ty ] Eio.Resource.t
  ; stdin : [> Eio.Flow.source_ty ] Eio.Resource.t
  ; .. >
  as
  'a

module S = Irmin_git_unix.FS.KV (History)

let run config env store =
  let store = Store.Store ((module S), store) in
  let initial_store = Engine.init env#fs env#process_mgr store in
  let rec loop store exit_code =
    try
      let prompt = Engine.prompt env exit_code store in
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
  |> List.filter (fun s -> not (String.equal s String.empty))
  |> List.map Engine.action_of_command

let main config env directory command_file =
  let conf = Irmin_git.config (Eio.Path.native_exn directory) in
  let repo = S.Repo.v conf in
  let store = S.main repo in
  match command_file with
  | Some file -> (
      let actions = command_file_to_actions file in
      let store = Store.Store ((module S), store) in
      let initial_store = Engine.init env#fs env#process_mgr store in
      let folder (store, exit_code) action =
        Fmt.pr "%a\n%!" Fmt.(styled (`Fg `Cyan) (Repr.pp Engine.action)) action;
        if exit_code <> `Exited 0 then (store, exit_code)
        else
          match Engine.run config env store action with
          | Error (`Process (Eio.Process.Child_error exit_code)) ->
              Fmt.pr "%a\n%!" Eio.Process.pp_status exit_code;
              (store, exit_code)
          | Error (`Process (Eio.Process.Executable_not_found m)) ->
              Fmt.pr "shelter: excutable not found %s\n%!" m;
              (store, `Exited 127)
          | Error (`Shell e) ->
              Fmt.pr "shelter: %a\n%!" Engine.pp_error e;
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
