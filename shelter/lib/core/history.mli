type mode = Void.mode

val mode_t : mode Repr.t

type post = { diff : Diff.t; time : int64; tracelog : Tracelog.t }
[@@deriving repr]

type pre = {
  mode : Void.mode;
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

val pre :
  ?mode:mode ->
  ?args:string list ->
  ?env:string list ->
  ?cwd:string ->
  ?user:int * int ->
  Zfs_store.Build.t ->
  pre

val post : ?diff:Diff.t -> ?tracelog:Tracelog.t -> int64 -> post
val v : pre -> post -> entry

val with_pre :
  ?mode:mode ->
  ?args:string list ->
  ?env:string list ->
  ?cwd:string ->
  ?user:int * int ->
  ?build:Zfs_store.Build.t ->
  pre ->
  pre

val with_post :
  ?diff:Diff.t -> ?tracelog:Tracelog.t -> ?time:int64 -> post -> post

val latest : t -> entry
val empty : t
val pp : t Fmt.t

include Irmin.Contents.S with type t := t
