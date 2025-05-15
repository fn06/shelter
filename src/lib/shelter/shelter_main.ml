open Eio
module Store = Store
module H = Shelter.History

type error = string

let pp_error = Fmt.string

module History = struct
  type mode = Void.mode

  let mode_t =
    Repr.map Repr.string
      (function
        | "R" -> Void.R | "RW" -> Void.RW | _ -> failwith "Malformed Void.mode")
      (function Void.R -> "R" | Void.RW -> "RW")

  type post = { diff : Diff.t; time : int64 } [@@deriving repr]

  type pre = {
    mode : mode;
    build : Store.Build.t;
    args : string list;
    env : string list;
    cwd : string;
    user : int * int;
  }
  [@@deriving repr]
  (** Needed for execution *)

  type t = { pre : pre; post : post } [@@deriving repr]

  let merge = Irmin.Merge.(default (Repr.option t))
end

type config = Config.t

let config_term = Config.cmdliner

type entry = History.t

type action =
  (* Change modes *)
  | Set_mode of History.mode
  (* Fork a new branch from an existing one,
     or switch to a branch if it exists *)
  | Set_session of string
  (* Run a command *)
  | Exec of string list
  (* Undo the last command *)
  | Undo
  (* Replay the current branch onto another *)
  | Replay of string
  (* Display info *)
  | Info of [ `Current | `History ]
  (* Error state *)
  | Unknown of string list
[@@deriving repr]

let split_and_remove_empty s =
  String.split_on_char ' ' s |> List.filter (fun v -> not (String.equal "" v))

let action = action_t

let shelter_action = function
  | "mode" :: [ "r" ] -> Set_mode R
  | "mode" :: [ "rw" ] -> Set_mode RW
  | "session" :: [ m ] -> Set_session m
  | "replay" :: [ onto ] -> Replay onto
  | [ "info" ] -> Info `Current
  | [ "undo" ] -> Undo
  | [ "history" ] -> Info `History
  | other -> Unknown other

let action_of_command cmd =
  match split_and_remove_empty cmd with
  | "@" :: rest -> shelter_action rest
  | args -> Exec args

let () = Fmt.set_style_renderer Format.str_formatter `Ansi_tty
let history_key = [ "history" ]
let key clock = history_key @ [ string_of_float @@ Eio.Time.now clock ]

let history (H.Store ((module S), store) : entry H.t) =
  let repo = S.repo store in
  match S.Head.find store with
  | None -> []
  | Some hd ->
      let rec linearize c =
        match S.Commit.parents c |> List.map (S.Commit.of_hash repo) with
        | [ Some p ] -> c :: linearize p
        | _ -> [ c ]
      in
      let commits = linearize hd in
      let get_diff_content t1 t2 =
        match S.Tree.diff t1 t2 with
        | [ (_, `Added (c, _)) ] -> c
        | lst ->
            let pp_diff =
              Repr.pp (Irmin.Diff.t (Repr.pair History.t S.metadata_t))
            in
            Fmt.epr "Get diff (%i) content %a%!" (List.length lst)
              Fmt.(list ~sep:Fmt.comma (Fmt.pair (Repr.pp S.path_t) pp_diff))
              lst;
            invalid_arg "Get diff should only have a single difference."
      in
      let hash c = S.Commit.hash c |> S.Hash.to_raw_string in
      let rec diff_calc = function
        | [] -> []
        | [ x ] ->
            let diff = get_diff_content (S.Tree.empty ()) (S.Commit.tree x) in
            [ (hash x, diff) ]
        | c :: p :: rest ->
            let diff = get_diff_content (S.Commit.tree p) (S.Commit.tree c) in
            (hash c, diff) :: diff_calc (p :: rest)
      in
      diff_calc commits

let with_latest ~default s f =
  match history s with [] -> default () | (_, hd) :: _ -> f hd

let text c = Fmt.(styled (`Fg c) string)

let sessions (H.Store ((module S), store) : entry H.t) =
  S.Branch.list (S.repo store)

let commit ~message clock (H.Store ((module S), store) : entry H.t) v =
  let info () = S.Info.v ~message (Eio.Time.now clock |> Int64.of_float) in
  S.set_exn ~info store (key clock) v

let which_branch ((H.Store ((module S), session) : entry H.t) as s) =
  let branches = sessions s in
  let repo = S.repo session in
  let heads = List.map (fun b -> (S.Branch.find repo b, b)) branches in
  let head = S.Head.find session in
  let head_hash =
    Option.map
      (fun hash -> String.sub (Fmt.str "%a" S.Commit.pp_hash hash) 0 7)
      head
  in
  (head_hash, List.assoc_opt head heads)

(* Reset the head of the current session by one commit *)
let reset_hard ((H.Store ((module S), session) : entry H.t) as s) =
  match
    List.filter_map (S.Commit.of_hash (S.repo session))
    @@ S.Commit.parents (S.Head.get session)
  with
  | [] -> s
  | p :: _ ->
      S.Head.set session p;
      s

(* Fork a new session from an existing one *)
let fork (H.Store ((module S), session) : entry H.t) new_branch =
  let repo = S.repo session in
  match (S.Head.find session, S.Branch.find repo new_branch) with
  | _, Some _ ->
      Error (new_branch ^ " already exists, try @ session " ^ new_branch)
  | None, _ -> Error "Current branch needs at least one commit"
  | Some commit, None ->
      let new_store = S.of_branch (S.repo session) new_branch in
      S.Branch.set repo new_branch commit;
      let store = H.Store ((module S), new_store) in
      Ok store

(* Fork a new session from an existing one *)
let display_history (s : entry H.t) =
  let pp_diff fmt d =
    if d = [] then () else Fmt.pf fmt "\n  %a" (Repr.pp Diff.t) d
  in
  let pp_entry fmt (e : entry) =
    Fmt.pf fmt "%-10s %s%a"
      Fmt.(str "%a" (styled (`Fg `Yellow) uint64_ns_span) e.post.time)
      (String.concat " " e.pre.args)
      pp_diff e.post.diff
  in
  let entries = history s |> List.rev in
  List.iter (fun (_hash, c) -> Fmt.pr "%a\n%!" pp_entry c) entries

let prompt status ((H.Store ((module S), _session) : entry H.t) as store) =
  let head, sesh = which_branch store in
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
  let prompt_entry (e : entry) =
    Fmt.pf Format.str_formatter "%a%a%a : { mode: %a }> " pp_status status
      (text `Yellow) "shelter" pp_sesh sesh (text `Red)
      (if e.pre.mode = R then "r" else "rw");
    Format.flush_str_formatter ()
  in
  with_latest store ~default:prompt prompt_entry

type ctx = { store : Store.t; tool_dir : string }

let tools = [ ("opentrace", Tools.opentrace) ]

let init fs proc s =
  let store = Store.init fs proc "shelter" in
  List.iter
    (fun (_, { History.pre = { History.args; _ }; _ }) ->
      LNoise.history_add (String.concat " " args) |> ignore)
    (history s);
  let tool_cid = Store.cid (String.concat ":" (List.map snd tools)) in
  let tools =
    Store.Run.with_tool store tool_cid @@ fun tool_dir ->
    Eio.Fiber.List.iter
      (fun (toolname, content) ->
        let new_path = Eio.Path.(fs / tool_dir / toolname) in
        Eio.Path.save ~create:(`If_missing 0o755) new_path content)
      tools;
    tool_dir
  in
  { store; tool_dir = tools }

(* Run a command:
  
   - TODO: pretty confusing that we `entry` to build from and also as the
     thing we are building (e.g. the build field and the args field... *)
let exec (config : config) ~stdout fs proc
    ((H.Store ((module S), _) : entry H.t), (ctx : ctx)) (entry : entry) =
  let build, env, (uid, gid) =
    match entry.pre.build with
    | Store.Build.Image img ->
        let build, env, user = Store.fetch ctx.store img in
        (build, env, Option.value ~default:(0, 0) user)
    | Store.Build.Build cid -> (cid, entry.pre.env, entry.pre.user)
  in
  let command = entry.pre.args in
  let hash_entry =
    { entry with pre = { entry.pre with build = Build build } }
  in
  (* Store things under History.pre, this makes it possible to rediscover
     the hash for something purely from the arguments needed to execute something
     rather than needing, for example, the time it took to execute!

     Also, combine it with previous build step. *)
  let new_cid =
    Store.cid (Cid.to_string build ^ Repr.to_string History.pre_t hash_entry.pre)
  in
  let with_rootfs fn =
    if entry.pre.mode = R then (Store.Run.with_build ctx.store build fn, [])
    else
      let diff_path =
        Eio.Path.(fs / Filename.temp_dir "shelter-diff-" "" / "diff")
      in
      Store.Run.with_clone ctx.store ~src:build new_cid diff_path fn
  in
  with_rootfs @@ function
  | `Exists path ->
      (* Copy the stdout log to stdout *)
      let () =
        Eio.Path.(with_open_in (fs / (path :> string) / "log")) @@ fun ic ->
        Eio.Flow.copy ic stdout
      in
      let c = Eio.Path.(load (fs / (path :> string) / "hash")) in
      Ok (`Reset c)
  | `Build rootfs ->
      let spawn sw log =
        if config.no_runc then
          (* Experiment Void Process *)
          let rootfs = Filename.concat rootfs "rootfs" in
          let void =
            Void.empty
            |> Void.rootfs ~mode:entry.pre.mode rootfs
            |> Void.cwd entry.pre.cwd
            (* TODO: Support UIDs |> Void.uid 1000 *)
            |> Void.exec ~env
                 [
                   config.shell;
                   "-c";
                   String.concat " " command ^ " && env > /tmp/shelter-env";
                 ]
          in
          `Void (Void.spawn ~sw void |> Void.exit_status)
        else
          let tool_mount : Runc.Json_config.mount =
            {
              ty = `Bind;
              src = ctx.tool_dir;
              dst = "/shelter-tools";
              readonly = true;
            }
          in
          let config =
            Runc.Json_config.
              {
                cwd = entry.pre.cwd;
                argv =
                  [
                    config.shell;
                    "-c";
                    String.concat " " command ^ " && env > /tmp/shelter-env";
                  ];
                hostname = "builder";
                network = [ "host" ];
                user = (uid, gid);
                env = entry.pre.env;
                mounts = [ tool_mount ];
                entrypoint = None;
              }
          in
          let env =
            object
              method fs = fs
              method proc = proc
              method stdout = stdout
            end
          in
          `Runc (Runc.spawn ~sw log env config rootfs)
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
      let start, res =
        Switch.run @@ fun sw ->
        let log =
          Eio.Path.open_out ~sw ~create:(`Or_truncate 0o644)
            Eio.Path.(fs / rootfs / "log")
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
      if res = `Exited 0 then (
        (* Extract env *)
        let env_path =
          Eio.Path.(fs / rootfs / "rootfs" / "tmp" / "shelter-env")
        in
        let env =
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
            env
          |> Option.value ~default:hash_entry.pre.cwd
        in
        if entry.pre.mode = RW then
          Ok
            (`Entry
               ( {
                   hash_entry with
                   History.pre =
                     {
                       hash_entry.pre with
                       build = Build new_cid;
                       env;
                       cwd;
                       user = (uid, gid);
                     };
                 },
                 rootfs ))
        else
          Ok
            (`Entry
               ( {
                   pre = { hash_entry.pre with cwd; env; user = (uid, gid) };
                   post = { hash_entry.post with time };
                 },
                 rootfs )))
      else Shelter.process_error (Eio.Process.Child_error res)

let complete_exec ((H.Store ((module S), store) as s : entry H.t), ctx) clock fs
    new_entry diff =
  match new_entry with
  | Error e -> Error e
  | Ok (`Reset c) -> (
      match
        S.Hash.unsafe_of_raw_string c |> S.Commit.of_hash (S.repo store)
      with
      | None ->
          Fmt.epr "Resetting to existing entry failed...\n%!";
          Ok (s, ctx)
      | Some c ->
          S.Head.set store c;
          Ok (s, ctx))
  | Ok (`Entry (entry, path)) ->
      (* Set diff *)
      let entry = History.{ entry with post = { entry.post with diff } } in
      (* Commit if RW *)
      if entry.pre.mode = RW then (
        commit
          ~message:("exec " ^ String.concat " " entry.pre.args)
          clock s entry;
        (* Save the commit hash for easy restoring later *)
        let hash = S.Head.get store |> S.Commit.hash |> S.Hash.to_raw_string in
        Eio.Path.save ~create:(`If_missing 0o644)
          Eio.Path.(fs / path / "hash")
          hash);
      Ok (s, ctx)

let replay config (H.Store ((module S), s) as store : entry H.t) ctx fs clock
    proc stdout existing_branch =
  let seshes = sessions store in
  if not (List.exists (String.equal existing_branch) seshes) then (
    Fmt.epr "%s does not exist!" existing_branch;
    Ok (store, ctx))
  else
    let repo = S.repo s in
    let onto = S.of_branch repo existing_branch in
    match S.lcas ~n:1 s onto with
    | Error lcas_error ->
        Fmt.epr "Replay LCAS: %a" (Repr.pp S.lca_error_t) lcas_error;
        Ok (store, ctx)
    | Ok [ lcas ] -> (
        let all_commits = history store in
        let lcas_hash = S.Commit.hash lcas |> S.Hash.to_raw_string in
        let rec collect = function
          | [] -> []
          | (x, _) :: _ when String.equal lcas_hash x -> []
          | v :: vs -> v :: collect vs
        in
        let commits_to_apply = collect all_commits in
        match commits_to_apply with
        | [] -> Shelter.shell_error ""
        | (h, first) :: rest ->
            let _, last_other =
              history (H.Store ((module S), onto)) |> List.hd
            in
            let new_first =
              {
                first with
                pre = { first.pre with build = last_other.pre.build };
              }
            in
            let commits_to_apply = (h, new_first) :: rest in
            (* Now we reset our head to point to the other store's head
           and replay our commits onto it *)
            let other_head = S.Head.get onto in
            S.Head.set s other_head;
            let res =
              List.fold_left
                (fun last (_, (entry : entry)) ->
                  match last with
                  | Error _ as e -> e
                  | Ok (new_store, new_ctx) ->
                      let new_entry, diff =
                        exec config ~stdout fs proc (new_store, new_ctx) entry
                      in
                      complete_exec (new_store, new_ctx) clock fs new_entry diff)
                (Ok (H.Store ((module S), s), ctx))
                commits_to_apply
            in
            res)
    | _ -> assert false (* Because n = 1 *)

let run (config : config) ~stdout fs clock proc
    (((H.Store ((module S), store) : entry H.t) as s), (ctx : ctx)) = function
  | Set_mode mode ->
      with_latest ~default:(fun _ -> Ok (s, ctx)) s @@ fun entry ->
      commit ~message:"mode change" clock s
        { entry with pre = { entry.pre with mode } };
      Ok (s, ctx)
  | Set_session m -> (
      (* Either set the session if the branch exists or create a new branch
         from the latest commit of the current branch *)
      let sessions = sessions s in
      match List.exists (String.equal m) sessions with
      | true ->
          let sesh = S.of_branch (S.repo store) m in
          Ok (H.Store ((module S), sesh), ctx)
      | false -> (
          match fork s m with
          | Error err ->
              Fmt.pr "[fork]: %a\n%!" (text `Red) err;
              Ok (s, ctx)
          | Ok store -> Ok (store, ctx)))
  | Unknown args ->
      Fmt.epr "%a" (text `Red) "Unknown Shelter Action\n";
      Shelter.shell_error (String.concat " " args)
  | Info `Current ->
      let sessions = sessions s in
      let sesh = Option.value ~default:"main" (snd (which_branch s)) in
      let history = history s in
      let pp_commit fmt (hash, msg) =
        Fmt.pf fmt "[%a]: %s" (text `Yellow) hash msg
      in
      let repo = S.repo store in
      let commits =
        List.fold_left
          (fun acc (commit, _) ->
            let commit =
              S.Hash.unsafe_of_raw_string commit
              |> S.Commit.of_hash repo |> Option.get
            in
            let info = S.Commit.info commit |> S.Info.message in
            let hash = S.Commit.hash commit |> Repr.to_string S.Hash.t in
            (String.sub hash 0 7, info) :: acc)
          [] history
      in
      let latest =
        with_latest
          ~default:(fun () -> None)
          s
          (fun e -> Some (Repr.to_string Store.Build.t e.pre.build))
      in
      Fmt.pr "Sessions: %a\nCurrent: %a\nHash: %a\nCommits:@.  %a\n%!"
        Fmt.(list ~sep:(Fmt.any ", ") string)
        sessions (text `Green) sesh
        Fmt.(option string)
        latest
        Fmt.(vbox ~indent:2 @@ list pp_commit)
        commits;
      Ok (s, ctx)
  | Exec [] -> Ok (s, ctx)
  | Undo -> Ok (reset_hard s, ctx)
  | Replay branch -> replay config s ctx fs clock proc stdout branch
  | Info `History ->
      display_history s;
      Ok (s, ctx)
  | Exec command -> (
      let entry =
        with_latest
          ~default:(fun () ->
            History.
              {
                pre =
                  {
                    mode = Void.RW;
                    build = Store.Build.Image config.image;
                    args = command;
                    (* TODO: extract with fetch *)
                    env = [];
                    cwd = "/";
                    user = (0, 0);
                  };
                post = { diff = []; time = 0L };
              })
          s Fun.id
      in
      let entry = { entry with pre = { entry.pre with args = command } } in
      try
        let new_entry, diff = exec config ~stdout fs proc (s, ctx) entry in
        complete_exec (s, ctx) clock fs new_entry diff
      with Eio.Exn.Io (Eio.Process.E e, _) -> Shelter.process_error e)
