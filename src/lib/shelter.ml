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
          match Action.of_string input with
          | None -> loop store (`Exited 0)
          | Some action -> (
              let _ : (unit, string) result =
                LNoise.history_add (Fmt.str "%a" Action.pp action)
              in
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
              | Ok store -> loop store (`Exited 0)))
    with Sys.Break -> loop store (`Exited 130)
  in
  loop initial_store (`Exited 0)

let handle_run store = function
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

module Context = Map.Make (String)

let map_iterable _store f (i : Shl.Ast.iterable) =
  match i with List m -> List.map f m | _ -> failwith "Map todo"

let re =
  Re.compile
    (Re.seq
       [ Re.char '#'; Re.group (Re.rep1 (Re.alt [ Re.alnum; Re.char '_' ])) ])

let replace_vars ctx s =
  Re.replace re
    ~f:(fun g ->
      let varname = Re.Group.get g 1 in
      (* group 1 is after '#' *)
      match Context.find_opt varname ctx with
      | Some value -> value
      | None -> "#" ^ varname (* keep original if no mapping *))
    s

let rec execute_ast env
    ((ctx, store, exit_code) :
      string Context.t * Store.t * Eio.Process.exit_status) run e =
  match e with
  | Shl.Ast.Action { v = cmd; _ } ->
      let cmd = replace_vars ctx cmd in
      run store (Action.of_string cmd |> Option.get) |> handle_run store
      |> fun (s, e) -> (ctx, s, e)
  | Shl.Ast.Meta { v = cmd; _ } ->
      let cmd = replace_vars ctx cmd in
      run store (Action.of_string ("@ " ^ cmd) |> Option.get)
      |> handle_run store
      |> fun (s, e) -> (ctx, s, e)
  | Shl.Ast.For { v = { for_; in_; body }; _ } -> (
      let contexts = map_iterable store (fun v -> Context.add for_ v ctx) in_ in
      match Store.which_branch store env with
      | _, Some name ->
          let results =
            List.map
              (fun ctx ->
                let new_branch = name ^ string_of_int @@ Random.int 100000001 in
                match Store.fork env store ~new_branch with
                | Error (`Msg m) -> failwith ("Failed to fork branch: " ^ m)
                | Ok for_branch ->
                    ( new_branch,
                      execute env (ctx, for_branch, exit_code) run body ))
              contexts
          in
          List.iter
            (fun (new_branch, _) ->
              Eio.traceln "merging %s" new_branch;
              match Store.merge store env new_branch with
              | Ok () -> ()
              | Error c ->
                  Fmt.failwith "for merge: %a"
                    (Repr.pp Irmin.Merge.conflict_t)
                    c)
            results;
          (ctx, store, exit_code)
      | _ -> failwith "No base branch!")
  | Shl.Ast.If_then_else { v = { if_; then_; else_ }; _ } -> (
      match
        run store
          (Action.Check (Action.split_and_remove_empty @@ Shl.Ast.value if_))
        |> handle_run store
      with
      | store, `Exited n when n <> 0 ->
          execute env (ctx, store, `Exited n) run else_
      | store, `Exited 0 -> execute env (ctx, store, `Exited 0) run then_
      | _ -> failwith "TODO")
  | Shl.Ast.Newline -> (ctx, store, exit_code)

and execute env
    ((ctx, store, exit_code) :
      string Context.t * Store.t * Eio.Process.exit_status) run = function
  | [] -> (ctx, store, exit_code)
  | e :: es -> (
      match execute_ast env (ctx, store, exit_code) run e with
      | (_, _, `Exited 0) as result -> execute env result run es
      | _, _, `Exited n ->
          Fmt.epr "Failed (%i) executing: %a\n%!" n Shl.Ast.pp [ e ];
          exit n
      | _, _, `Signaled n ->
          Fmt.epr "Signaled %i\n%!" n;
          exit n)

let main config env directory shl_file =
  let conf = Irmin_git.config (Eio.Path.native_exn directory) in
  let repo = S.Repo.v conf in
  let store = S.main repo in
  match shl_file with
  | Some file -> (
      let filename = Eio.Path.native_exn file |> Filename.basename in
      let ast = Shl.of_src ~filename (`String (Eio.Path.load file)) in
      let store = Store.Store ((module S), store) in
      let initial_store = Engine.init env#fs env#process_mgr store in
      let _cxt, _store, exit_code =
        execute env
          (Context.empty, initial_store, `Exited 0)
          (Engine.run config env) ast
      in
      match exit_code with
      | `Exited 0 -> ()
      | `Exited n | `Signaled n ->
          Fmt.epr "%a\n%!" Eio.Process.pp_status exit_code;
          exit n)
  | None -> run config env store
