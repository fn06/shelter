type mode = Void.mode

val mode_t : mode Repr.t

type post = { diff : Diff.t; time : int64; tracelog : Tracelog.t }
[@@deriving repr]

type pre = {
  mode : Void.mode;
  build : Store.Build.t;
  args : string list;
  env : string list;
  cwd : string;
  user : int * int;
}
[@@deriving repr]
(** Needed for execution *)

type t = { pre : pre; post : post } [@@deriving repr]

include Irmin.Contents.S with type t := t
