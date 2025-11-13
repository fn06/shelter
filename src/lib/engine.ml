open Eio

let ( / ) = Eio.Path.( / )

module History = History
module Store = Store

type error = [ `Msg of string ]

let pp_error fmt (`Msg err) = Fmt.string fmt err

type config = Config.t

let config_term = Config.cmdliner

type contents = History.t

let () = Fmt.set_style_renderer Format.str_formatter `Ansi_tty
let text c = Logger.pp_colored c Fmt.string
let pp_commit fmt (hash, msg) = Fmt.pf fmt "[%a]: %s" (text `Yellow) hash msg

let pp_cid fmt cid =
  Fmt.pf fmt "%a" (text `Yellow) (String.sub (Cid.to_string cid) 0 12 ^ "...")

let prompt env status store =
  let head, sesh = Store.which_branch store env in
  let sesh = Option.value ~default:"main" sesh in
  let prompt () =
    Fmt.(styled (`Fg `Yellow) string) Format.str_formatter "shelter> ";
    Format.flush_str_formatter ()
  in
  let pp_head fmt = function
    | None -> Fmt.nop fmt ()
    | Some h -> Fmt.pf fmt "#%a" (text `Magenta) h
  in
  let pp_sesh fmt sesh = Fmt.pf fmt "[%a%a]" (text `Green) sesh pp_head head in
  let pp_status fmt = function
    | `Exited 0 -> Fmt.nop fmt ()
    | `Exited n -> Fmt.pf fmt "%a " (text `Red) (string_of_int n)
    | _ -> Fmt.nop fmt ()
  in
  let prompt_entry (e : contents) =
    let hd = History.latest e in
    Fmt.pf Format.str_formatter "%a%a%a : { mode: %a }> " pp_status status
      (text `Yellow) "shelter" pp_sesh sesh (text `Red)
      (if hd.pre.mode = R then "r" else "rw");
    Format.flush_str_formatter ()
  in
  Store.with_latest (Store.get_store store) ~default:prompt prompt_entry

type ctx = Store.ctx
type store = Store.t

let ctx = Store.get_ctx
let history = Store.get_store
let with_latest_iter = Store.with_latest ~default:(fun () -> ())

let init fs proc s =
  let ctx = Zfs_store.init fs proc "shelter" in
  let f =
    List.iter (fun { History.pre = { History.args; _ }; _ } ->
        LNoise.history_add (String.concat " " args) |> ignore)
  in
  with_latest_iter s f;
  Store.v s ctx

(* Run a command:
  
   - TODO: pretty confusing that we `entry` to build from and also as the
     thing we are building (e.g. the build field and the args field... *)
let exec (config : config) env (s : Store.t) (entry : History.entry) =
  let build, environ, shell, (uid, gid) =
    match entry.pre.build with
    | Zfs_store.Build.Image img ->
        let build, env, cmd, user = Zfs_store.fetch s.ctx img in
        ( build,
          env,
          List.hd cmd |> String.trim,
          Option.value ~default:(0, 0) user )
    | Zfs_store.Build.Build cid ->
        (cid, entry.pre.env, entry.pre.shell, entry.pre.user)
  in
  let command = entry.pre.args in
  let hash_entry =
    let pre =
      History.with_pre ~env:environ ~build:(Build build) ~shell entry.pre
    in
    History.v pre entry.post
  in
  (* Store things under History.pre, this makes it possible to rediscover
     the hash for something purely from the arguments needed to execute something
     rather than needing, for example, the time it took to execute!

     Also, combine it with previous build step. *)
  let new_cid =
    Zfs_store.cid
      (Cid.to_string build ^ Repr.to_string History.pre_t hash_entry.pre)
  in
  let with_rootfs fn =
    if entry.pre.mode = R then
      (Zfs_store.Run.with_build ~overlays:entry.overlays s.ctx build fn, [])
    else
      let diff_path =
        Eio.Path.(env#fs / Filename.temp_dir "shelter-diff-" "" / "diff")
      in
      Zfs_store.Run.with_clone ~overlays:entry.overlays s.ctx ~src:build new_cid
        diff_path fn
  in
  with_rootfs @@ function
  | `Exists path ->
      (* Copy the stdout log to stdout *)
      Fmt.epr "[%a] %a\n%!" pp_cid new_cid
        (Fmt.styled (`Fg `Cyan) @@ Fmt.(list ~sep:Fmt.(any " ") string))
        entry.pre.args;
      let () =
        Eio.Path.(with_open_in (env#fs / (path :> string) / "log")) @@ fun ic ->
        Logger.pp_rolling_window
          ~pp_line:(Logger.pp_colored `White Fmt.string)
          Fmt.stdout ic
      in
      let c = Eio.Path.(load (env#fs / (path :> string) / "hash")) in
      Ok (`Reset c)
  | `Build rootfs ->
      let trace_log = Buffer.create 128 in
      let shell =
        match config.shell with Some shell -> shell | None -> shell
      in
      let spawn sw log =
        if config.no_runc then
          (* Experiment Void Process *)
          let rootfs = Filename.concat rootfs "rootfs" in
          let void =
            Void.empty
            |> Void.rootfs ~mode:entry.pre.mode rootfs
            |> Void.cwd entry.pre.cwd
            (* TODO: Support UIDs |> Void.uid 1000 *)
            |> Void.exec ~env:environ
                 [
                   shell;
                   "-c";
                   String.concat " " command ^ " && env > /tmp/shelter-env";
                 ]
          in
          `Void (Void.spawn ~sw void |> Void.exit_status)
        else
          let config =
            Runc.Json_config.
              {
                cwd = entry.pre.cwd;
                argv =
                  (* TODO: Workaround for exit_status pain with runc start *)
                  [
                    shell;
                    "-c";
                    Fmt.str "(%s)" (String.concat " " command)
                    ^ "; status=$?; echo $status > /tmp/shelter-status; env > \
                       /tmp/shelter-env; exit $status";
                  ];
                hostname = "builder";
                network = [ "host" ];
                user = (uid, gid);
                env = environ;
                mounts = [];
                entrypoint = None;
              }
          in
          `Runc
            (Runc.spawn ~sw ~has_overlays:(entry.overlays <> [])
               ~before_start:(fun id ->
                 (* Set up opentrace on the container's cgroup *)
                 let pid =
                   Eio.Process.parse_out env#process_mgr Eio.Buf_read.take_all
                     [ "runc"; "state"; id ]
                   |> Yojson.Safe.from_string
                   |> Yojson.Safe.Util.member "pid"
                   |> Yojson.Safe.Util.to_int
                 in
                 let cgroup =
                   Eio.Process.parse_out env#process_mgr Eio.Buf_read.take_all
                     [ "cat"; Fmt.str "/proc/%i/cgroup" pid ]
                   |> Astring.String.cut ~sep:"::"
                   |> function
                   | Some (_, path) -> String.trim path
                   | None -> Fmt.failwith "Failed to find cgroup for %i" pid
                 in
                 Eio.Fiber.fork_daemon ~sw (fun () ->
                     Eio.Switch.run @@ fun sw ->
                     let _ =
                       Eio.Process.spawn ~sw
                         ~stdout:(Eio.Flow.buffer_sink trace_log)
                         env#process_mgr
                         [
                           "bpftrace";
                           "-B";
                           "none";
                           "-e";
                           {|tracepoint:syscalls:sys_enter_open / cgroup == |}
                           ^ Fmt.str "cgroupid(\"/sys/fs/cgroup/%s\")" cgroup
                           ^ {|/ { printf("open,%s,%s,0x%x\n", str(args->filename), comm, args->flags); } |}
                           ^ {|tracepoint:syscalls:sys_enter_openat / cgroup == |}
                           ^ Fmt.str "cgroupid(\"/sys/fs/cgroup/%s\")" cgroup
                           ^ {|/ { printf("openat,%s,%s,0x%x\n", str(args->filename), comm, args->flags); } |}
                           ^ {|tracepoint:syscalls:sys_enter_openat2 / cgroup == |}
                           ^ Fmt.str "cgroupid(\"/sys/fs/cgroup/%s\")" cgroup
                           ^ {|/ { printf("openat2,%s,%s,0x%x\n", str(args->filename), comm, args.how->flags); } |};
                         ]
                     in
                     `Stop_daemon);
                 Eio_unix.sleep 0.3)
               log env config rootfs)
      in
      let savedTio = Unix.tcgetattr Unix.stdin in
      let tio =
        {
          savedTio with
          (* input modes *)
          c_ignpar = true;
          c_istrip = false;
          c_inlcr = false;
          c_igncr = false;
          c_ixon = false;
          (* c_ixany = false; *)
          (* c_iuclc = false; *)
          c_ixoff = false;
          (* output modes *)
          c_opost = false;
          (* control modes *)
          c_isig = false;
          c_icanon = false;
          c_echo = false;
          c_echoe = false;
          c_echok = false;
          c_echonl = false;
          (* c_iexten = false; *)

          (* special characters *)
          c_vmin = 1;
          c_vtime = 0;
        }
      in
      Unix.tcsetattr Unix.stdin TCSADRAIN tio;
      let start, _ =
        Switch.run @@ fun sw ->
        let log =
          Eio.Path.open_out ~sw ~create:(`Or_truncate 0o644)
            (env#fs / rootfs / "log")
        in
        let res = spawn sw log in
        let start = Mtime_clock.now () in
        match res with
        | `Runc r -> (start, Eio.Process.await r)
        | `Void v -> (start, Void.to_eio_status (Eio.Promise.await v))
      in

      (* restore tio *)
      Unix.tcsetattr Unix.stdin TCSADRAIN savedTio;

      let stop = Mtime_clock.now () in
      let span = Mtime.span start stop in
      let time = Mtime.Span.to_uint64_ns span in
      (* Add command to history regardless of exit status *)
      let _ : (unit, string) result =
        LNoise.history_add (String.concat " " command)
      in
      let status =
        Eio.Path.(load (env#fs / rootfs / "rootfs" / "tmp" / "shelter-status"))
        |> String.trim |> int_of_string
      in
      if status = 0 then (
        (* Extract env *)
        let env_path =
          Eio.Path.(env#fs / rootfs / "rootfs" / "tmp" / "shelter-env")
        in
        let environ =
          Eio.Path.(load env_path)
          |> String.split_on_char '\n'
          |> List.filter (fun s -> not (String.equal "" s))
        in
        Eio.Path.unlink env_path;
        let cwd =
          List.find_map
            (fun v ->
              match Astring.String.cut ~sep:"=" v with
              | Some ("PWD", dir) -> Some dir
              | _ -> None)
            environ
          |> Option.value ~default:hash_entry.pre.cwd
        in
        let tracelog = Tracelog.of_bpftrace (Buffer.contents trace_log) in
        let post = History.with_post ~time ~tracelog hash_entry.post in
        if entry.pre.mode = RW then
          let pre =
            History.with_pre ~build:(Build new_cid) ~env:environ ~cwd ~shell
              ~user:(uid, gid) hash_entry.pre
          in
          Ok (`Entry (History.v pre post, rootfs))
        else
          let pre =
            History.with_pre ~env:environ ~cwd ~user:(uid, gid) ~shell
              hash_entry.pre
          in
          Ok (`Entry (History.v pre post, rootfs)))
      else Error.process_error (Eio.Process.Child_error (`Exited status))

let run (config : config) env (s : Store.t) = function
  | Action.Set_mode mode ->
      Store.with_latest ~default:(fun _ -> Ok s) (Store.get_store s)
      @@ fun contents ->
      let entry = History.latest contents in
      Store.commit ~message:"mode change" env#clock s
        ({ entry with pre = { entry.pre with mode } } :: contents);
      Ok s
  | Session None ->
      let sessions = Store.sessions s in
      Fmt.pr "%a\n%!" Fmt.(list ~sep:(Fmt.any "\n") string) sessions;
      Ok s
  | Session (Some v) -> (
      (* Either set the session if the branch exists or create a new branch
         from the latest commit of the current branch *)
      let sessions = Store.sessions s in
      let rec new_name () =
        let name = Name_generator.new_name env#secure_random in
        if List.mem name sessions then new_name () else name
      in
      let name, image =
        match v with
        | Name n -> (n, None)
        | Image n -> (new_name (), Some n)
        | Name_and_image (n, i) -> (n, Some i)
      in
      match List.exists (String.equal name) sessions with
      | true -> Ok (Store.set_session env s name)
      | false -> (
          let detach = Option.is_some image in
          match Store.fork ~detach env s ~new_branch:name with
          | Error (`Msg err) ->
              Fmt.pr "[fork]: %a\n%!" (text `Red) err;
              Ok s
          | Ok store -> (
              match image with
              | None -> Ok store
              | Some img ->
                  (* We need to pull the image and set everything up for this
                 detached, new session *)
                  let pre = History.pre (Zfs_store.Build.Image img) in
                  let post = History.post 0L in
                  let entry = History.v pre post in
                  Store.commit ~message:(Fmt.str "from %s" img) env#clock store
                    [ entry ];
                  Ok store)))
  | Unknown args ->
      Fmt.epr "%a" (text `Red) "Unknown Shelter Action\n";
      Error.shell_error (`Msg (String.concat " " args))
  | Info `Current ->
      let sessions = Store.sessions s in
      let sesh =
        Option.value ~default:"main" (snd (Store.which_branch s env))
      in
      let commits = Store.commit_info s in
      let latest =
        Store.with_latest
          ~default:(fun () -> None)
          (Store.get_store s)
          (fun c ->
            let e = History.latest c in
            Some (Repr.to_string Zfs_store.Build.t e.pre.build))
      in
      Fmt.pr "Sessions: %a\nCurrent: %a\nHash: %a\nCommits:@.  %a\n%!"
        Fmt.(list ~sep:(Fmt.any ", ") string)
        sessions (text `Green) sesh
        Fmt.(option string)
        latest
        Fmt.(vbox ~indent:2 @@ list pp_commit)
        commits;
      Ok s
  | Exec [] -> Ok s
  | Undo n -> Ok (Store.reset_hard ~n s)
  | Replay branch -> Store.replay (exec config env) s env branch
  | Merge branch -> (
      match Store.merge s env branch with
      | Ok () -> Ok s
      | Error con ->
          Error.shell_error
            (`Msg
               (Fmt.str "Merged failed: %a"
                  (Repr.pp Irmin.Merge.conflict_t)
                  con)))
  | Info `History ->
      let entries =
        Store.with_latest ~default:(fun () -> []) (Store.get_store s) Fun.id
        |> List.rev
      in
      History.pp Fmt.stdout entries;
      Ok s
  | Exec command -> (
      let entry =
        Store.with_latest
          ~default:(fun () ->
            let pre = History.(pre (Image config.image) ~args:command) in
            let post = History.post 0L in
            [ History.v pre post ])
          (Store.get_store s) Fun.id
        |> History.latest
      in
      let entry =
        { entry with pre = History.with_pre ~args:command entry.pre }
      in
      try
        let new_entry, diff = exec config env s entry in
        Store.save_execution s env new_entry diff
      with Eio.Exn.Io (Eio.Process.E e, _) -> Error.process_error e)
  | Check command -> (
      let entry =
        Store.with_latest
          ~default:(fun () ->
            let pre = History.(pre (Image config.image) ~args:command) in
            let post = History.post 0L in
            [ History.v pre post ])
          (Store.get_store s) Fun.id
        |> History.latest
      in
      let entry = { entry with pre = { entry.pre with args = command } } in
      try
        let new_entry, _diff = exec config env s entry in
        Result.map (fun _ -> s) new_entry
      with Eio.Exn.Io (Eio.Process.E e, _) -> Error.process_error e)

open Cmdliner
(** Additional Commands *)

let cmd_file =
  let doc = "Path to a shelter file (e.g. run.shl)." in
  Arg.(value & opt (some file) None & info [ "f"; "file" ] ~docv:"FILE" ~doc)

let format_file =
  let run cmd_file =
    let src =
      match cmd_file with
      | None -> `In_channel In_channel.stdin
      | Some file ->
          let file = In_channel.with_open_bin file In_channel.input_all in
          `String file
    in
    Shl.format src
  in
  let t = Term.(const run $ cmd_file) in
  let info = Cmd.info "format" in
  Cmd.v info t

let cmds = [ format_file ]
