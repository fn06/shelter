type mode = Void.mode

let mode_t =
  Repr.map Repr.string
    (function
      | "R" -> Void.R | "RW" -> Void.RW | _ -> failwith "Malformed Void.mode")
    (function Void.R -> "R" | Void.RW -> "RW")

type post = { diff : Diff.t; time : int64; tracelog : Tracelog.t }
[@@deriving repr]

type pre = {
  mode : mode;
  build : Zfs_store.Build.t;
  args : string list;
  env : string list;
  cwd : string;
  user : int * int;
}
[@@deriving repr]
(** Needed for execution *)

type entry = { pre : pre; post : post; overlays : Zfs_store.Build.t list }
[@@deriving repr]

type t = entry list [@@deriving repr]

let pre ?(mode = Void.RW) ?(args = []) ?(env = []) ?(cwd = "/") ?(user = (0, 0))
    build =
  { mode; build; args; env; cwd; user }

let post ?(diff = []) ?(tracelog = Tracelog.empty) time =
  { time; tracelog; diff }

let v ?(overlays = []) pre post = { pre; post; overlays }

let with_pre ?mode ?args ?env ?cwd ?user ?build with_pre =
  {
    mode = Option.value ~default:with_pre.mode mode;
    args = Option.value ~default:with_pre.args args;
    env = Option.value ~default:with_pre.env env;
    cwd = Option.value ~default:with_pre.cwd cwd;
    user = Option.value ~default:with_pre.user user;
    build = Option.value ~default:with_pre.build build;
  }

let with_post ?diff ?tracelog ?time post =
  {
    time = Option.value ~default:post.time time;
    diff = Option.value ~default:post.diff diff;
    tracelog = Option.value ~default:post.tracelog tracelog;
  }

let latest = function [] -> invalid_arg "Empty history!" | x :: _ -> x
let empty = []

let merge_function ~old t1 t2 =
  (* By the design of the merge function these three histories
     will have a subset of commands that are the same ([old])
     and all of the new commands are what is left *)
  match old () with
  | Error _ as e -> e
  | Ok old ->
      let shared_cmds = Option.map List.length old |> Option.value ~default:0 in
      let t2_latest = latest t2 in
      let t1_latest = latest t1 in
      (* Drop the shared commands out of t1's history. *)
      let t1_rest =
        List.tl t1 |> List.rev
        |> List.filteri (fun i _ -> i >= shared_cmds)
        |> List.rev
      in
      let overlays = t2_latest.pre.build :: t2_latest.overlays in
      let new_t1_latest =
        { t1_latest with overlays = overlays @ t1_latest.overlays }
      in
      let merged = (new_t1_latest :: t1_rest) @ t2 in
      Ok merged

let merge = Irmin.Merge.option @@ Irmin.Merge.v t merge_function

let pp fmt entries =
  let pp_entry fmt (e : entry) =
    Fmt.pf fmt "%-10s %s\n%a"
      Fmt.(str "%a" (styled (`Fg `Yellow) uint64_ns_span) e.post.time)
      (String.concat " " e.pre.args)
      Diff.pp e.post.diff (* Tracelog.pp e.post.tracelog *)
  in
  List.iter (fun c -> Fmt.pf fmt "%a\n%!" pp_entry c) entries
