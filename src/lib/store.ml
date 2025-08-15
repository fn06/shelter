type 'entry irmin =
  | Store :
      ((module Irmin.S
          with type t = 'a
           and type Schema.Branch.t = string
           and type Schema.Contents.t = 'entry
           and type Schema.Path.t = string list
           and type Schema.Path.step = string)
      * 'a)
      -> 'entry irmin

type ctx = Zfs_store.t
type entry = History.entry
type store = entry list irmin

let history_key = [ "history" ]

type t = { store : store; ctx : ctx }

let get_store t = t.store
let get_ctx t = t.ctx
let v store ctx = { store; ctx }

let history (Store ((module S), store) : store) =
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
        | [ (_, `Updated ((_, _), (c, _))) ] -> c
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

let sessions { store = Store ((module S), store); _ } =
  S.Branch.list (S.repo store)

let commit ~message clock { store = Store ((module S), store); _ } v =
  let info () = S.Info.v ~message (Eio.Time.now clock |> Int64.of_float) in
  S.set_exn ~info store history_key v

let commit_info { store = Store ((module S), store); _ } =
  let history = history (Store ((module S), store)) in
  let repo = S.repo store in
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

(* Reset the head of the current session by one commit *)
let reset_hard ({ store = Store ((module S), store); _ } as s) =
  match
    List.filter_map (S.Commit.of_hash (S.repo store))
    @@ S.Commit.parents (S.Head.get store)
  with
  | [] -> s
  | p :: _ ->
      S.Head.set store p;
      s

let save_execution ({ store = Store ((module S), store); _ } as s) env new_entry
    diff =
  match new_entry with
  | Error e -> Error e
  | Ok (`Reset c) -> (
      match
        S.Hash.unsafe_of_raw_string c |> S.Commit.of_hash (S.repo store)
      with
      | None ->
          Fmt.epr "Resetting to existing entry failed...\n%!";
          Ok s
      | Some c ->
          S.Head.set store c;
          Ok s)
  | Ok (`Entry (entry, path)) ->
      (* Set diff *)
      let current = Option.value ~default:[] @@ S.find store history_key in
      let new_entry =
        History.(v entry.pre (History.with_post ~diff entry.post))
      in
      (* Commit if RW *)
      if entry.pre.mode = RW then (
        commit
          ~message:("exec " ^ String.concat " " entry.pre.args)
          env#clock s (new_entry :: current);
        (* Save the commit hash for easy restoring later *)
        let hash = S.Head.get store |> S.Commit.hash |> S.Hash.to_raw_string in
        Eio.Path.save ~create:(`If_missing 0o644)
          Eio.Path.(env#fs / path / "hash")
          hash);
      Ok s

let replay exec ({ store = Store ((module S), store); ctx } as s) env
    existing_branch =
  let seshes = sessions s in
  if not (List.exists (String.equal existing_branch) seshes) then (
    Fmt.epr "%s does not exist!" existing_branch;
    Ok s)
  else
    let repo = S.repo store in
    let onto = S.of_branch repo existing_branch in
    match S.lcas ~n:1 store onto with
    | Error lcas_error ->
        Fmt.epr "Replay LCAS: %a" (Repr.pp S.lca_error_t) lcas_error;
        Ok s
    | Ok [ lcas ] -> (
        let all_commits = history (get_store s) in
        let lcas_hash = S.Commit.hash lcas |> S.Hash.to_raw_string in
        let rec collect = function
          | [] -> []
          | (x, _) :: _ when String.equal lcas_hash x -> []
          | v :: vs -> v :: collect vs
        in
        let commits_to_apply = collect all_commits in
        match commits_to_apply with
        | [] -> Error.shell_error (`Msg "no commits")
        | (h, first) :: rest ->
            let _, last_other = history (Store ((module S), onto)) |> List.hd in
            let first = List.hd first in
            let last_other_head = List.hd last_other in
            let new_first =
              {
                first with
                pre = { first.pre with build = last_other_head.pre.build };
              }
              :: last_other
            in
            let commits_to_apply = (h, new_first) :: rest in
            (* Now we reset our head to point to the other store's head
           and replay our commits onto it *)
            let other_head = S.Head.get onto in
            S.Head.set store other_head;
            let res =
              List.fold_left
                (fun last (_, (entry : History.t)) ->
                  let entry_to_apply = History.latest entry in
                  match last with
                  | Error _ as e -> e
                  | Ok { store = new_store; ctx = new_ctx } ->
                      let new_entry, diff =
                        exec { store = new_store; ctx = new_ctx } entry_to_apply
                      in
                      save_execution
                        { store = new_store; ctx = new_ctx }
                        env new_entry diff)
                (Ok { store = Store ((module S), store); ctx })
                commits_to_apply
            in
            res)
    | _ -> assert false (* Because n = 1 *)

let merge { store = Store ((module S), store); _ } env branch_name =
  let message = Fmt.str "merged %s" branch_name in
  let info () = S.Info.v ~message (Eio.Time.now env#clock |> Int64.of_float) in
  S.merge_with_branch ~info store branch_name

let with_directory { store = Store ((module S), store); _ } env fn =
  let repo = S.repo store in
  let config = S.Repo.config repo in
  match Irmin.Backend.Conf.find_root config with
  | None -> failwith "No directory!"
  | Some p -> fn Eio.Path.(env#fs / p)

let save_branch_name s env ~name =
  with_directory s env @@ fun dir ->
  Eio.Path.(save ~create:(`If_missing 0o644) (dir / "runtime-branch") name)

let which_branch_from_file s env =
  with_directory s env @@ fun dir -> Eio.Path.(load (dir / "runtime-branch"))

let which_branch ({ store = Store ((module S), store); _ } as s) env =
  let branches = sessions s in
  let repo = S.repo store in
  let heads = List.map (fun b -> (S.Branch.find repo b, b)) branches in
  let head = S.Head.find store in
  let head_hash =
    Option.map
      (fun hash -> String.sub (Fmt.str "%a" S.Commit.pp_hash hash) 0 7)
      head
  in
  let branches =
    List.find_all
      (fun (h, _) ->
        Repr.(unstage (equal (Repr.option (S.Commit.t repo)))) head h)
      heads
  in
  match branches with
  | [] -> (head_hash, None)
  | [ (_, c) ] -> (head_hash, Some c)
  | _ ->
      (* The case where we have _just_ created a new branch and cannot
       distinguish it from the branch we created it from *)
      (head_hash, Some (which_branch_from_file s env))

(* New sessions *)
let set_session env ({ store = Store ((module S), store); ctx } as s) m =
  let sesh = S.of_branch (S.repo store) m in
  save_branch_name s env ~name:m;
  { store = Store ((module S), sesh); ctx }

let fork ?(detach = false) env
    ({ store = Store ((module S), session); ctx } as s) ~new_branch =
  let repo = S.repo session in
  match (S.Head.find session, S.Branch.find repo new_branch) with
  | _, Some _ ->
      Error (`Msg (new_branch ^ " already exists, try @ session " ^ new_branch))
  | None, _ -> Error (`Msg "Current branch needs at least one commit")
  | Some commit, None ->
      let new_store = S.of_branch (S.repo session) new_branch in
      if not detach then S.Branch.set repo new_branch commit;
      save_branch_name s env ~name:new_branch;
      let store = Store ((module S), new_store) in
      Ok { store; ctx }
