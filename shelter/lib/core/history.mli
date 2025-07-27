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

val pre :
  ?mode:mode ->
  ?args:string list ->
  ?env:string list ->
  ?cwd:string ->
  ?user:int * int ->
  Store.Build.t ->
  pre

val post : ?diff:Diff.t -> ?tracelog:Tracelog.t -> int64 -> post
val v : pre -> post -> t

val with_pre :
  ?mode:mode ->
  ?args:string list ->
  ?env:string list ->
  ?cwd:string ->
  ?user:int * int ->
  ?build:Store.Build.t ->
  pre ->
  pre

val with_post :
  ?diff:Diff.t -> ?tracelog:Tracelog.t -> ?time:int64 -> post -> post

val pp : t list Fmt.t

include Irmin.Contents.S with type t := t
