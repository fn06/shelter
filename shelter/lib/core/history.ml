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
