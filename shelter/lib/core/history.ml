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

type entry = { pre : pre; post : post } [@@deriving repr]
type t = entry list [@@deriving repr]

let pre ?(mode = Void.RW) ?(args = []) ?(env = []) ?(cwd = "/") ?(user = (0, 0))
    build =
  { mode; build; args; env; cwd; user }

let post ?(diff = []) ?(tracelog = Tracelog.empty) time =
  { time; tracelog; diff }

let v pre post = { pre; post }

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

let merge_function ~old:_ _t1 t2 = Ok t2
let merge = Irmin.Merge.option @@ Irmin.Merge.v t merge_function
let latest = function [] -> invalid_arg "Empty history!" | x :: _ -> x
let empty = []

let pp fmt entries =
  let pp_diff fmt d =
    if d = [] then Fmt.pf fmt "\nNo modifications to filesystem\n%!"
    else Fmt.pf fmt "\n%a\n%!" Diff.pp d
  in
  let pp_entry fmt (e : entry) =
    Fmt.pf fmt "%-10s %s%a%a\n"
      Fmt.(str "%a" (styled (`Fg `Yellow) uint64_ns_span) e.post.time)
      (String.concat " " e.pre.args)
      pp_diff e.post.diff Tracelog.pp e.post.tracelog
  in
  List.iter (fun c -> Fmt.pf fmt "%a\n%!" pp_entry c) entries
