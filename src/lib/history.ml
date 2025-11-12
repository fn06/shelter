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
  shell : string;
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
    ?(shell = "/bin/sh") build =
  { mode; build; args; env; cwd; user; shell }

let post ?(diff = []) ?(tracelog = Tracelog.empty) time =
  { time; tracelog; diff }

let v ?(overlays = []) pre post = { pre; post; overlays }

let with_pre ?mode ?args ?env ?cwd ?user ?shell ?build with_pre =
  {
    mode = Option.value ~default:with_pre.mode mode;
    args = Option.value ~default:with_pre.args args;
    env = Option.value ~default:with_pre.env env;
    cwd = Option.value ~default:with_pre.cwd cwd;
    user = Option.value ~default:with_pre.user user;
    build = Option.value ~default:with_pre.build build;
    shell = Option.value ~default:with_pre.shell shell;
  }

let with_post ?diff ?tracelog ?time post =
  {
    time = Option.value ~default:post.time time;
    diff = Option.value ~default:post.diff diff;
    tracelog = Option.value ~default:post.tracelog tracelog;
  }

let latest = function [] -> invalid_arg "Empty history!" | x :: _ -> x
let empty = []

module Files = Set.Make (String)

let merge_function ~old t1 t2 =
  (* By the design of the merge function these three histories
     will have a subset of commands that are the same ([old])
     and all of the new commands are what is left *)
  match old () with
  | Error _ as e -> e
  | Ok old -> (
      let shared_cmds = Option.map List.length old |> Option.value ~default:0 in
      let t2_latest = latest t2 in
      let t1_latest = latest t1 in
      (* First we check for a merge conflict: does t1 read anything that t2 might
         have written to! *)
      let t2_writes =
        List.concat_map (fun e -> Tracelog.writes e.post.tracelog) t2
        |> Files.of_list
      in
      (* Drop the shared commands out of t1's history. *)
      let t1_rest =
        List.rev t1 |> List.filteri (fun i _ -> i >= shared_cmds) |> List.rev
      in
      let t1_reads =
        List.concat_map (fun e -> Tracelog.reads e.post.tracelog) t1_rest
        |> Files.of_list
      in
      let overlap = Files.inter t2_writes t1_reads in
      match Files.is_empty overlap with
      | false ->
          let s =
            Fmt.str
              "read-write inconsistency: %a consider using %@ replay <br> \
               instead."
              Fmt.(braces (list ~sep:Fmt.comma string))
              (Files.to_list overlap)
          in
          Fmt.epr "%a\n%!" Fmt.(styled (`Fg `Red) string) s;
          Irmin.Merge.conflict "read-write inconsistency"
      | true ->
          let overlays = t2_latest.pre.build :: t2_latest.overlays in
          let new_t1_latest =
            { t1_latest with overlays = overlays @ t1_latest.overlays }
          in
          let merged = (new_t1_latest :: t1_rest) @ t2 in
          Ok merged)

let merge = Irmin.Merge.option @@ Irmin.Merge.v t merge_function

let pp fmt entries =
  let pp_entry fmt (e : entry) =
    Fmt.pf fmt "%-10s %s\n%a"
      Fmt.(str "%a" (styled (`Fg `Yellow) uint64_ns_span) e.post.time)
      (String.concat " " e.pre.args)
      Diff.pp e.post.diff (* Tracelog.pp e.post.tracelog *)
  in
  List.iter (fun c -> Fmt.pf fmt "%a\n%!" pp_entry c) entries
