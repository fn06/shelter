type mode = Void.mode

val mode_t : mode Repr.t

type post = { diff : Diff.t; time : int64; tracelog : Tracelog.t }
[@@deriving repr]

type pre = {
  mode : Void.mode;
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

val pre :
  ?mode:mode ->
  ?args:string list ->
  ?env:string list ->
  ?cwd:string ->
  ?user:int * int ->
  ?shell:string ->
  Zfs_store.Build.t ->
  pre
(** Constructs information needed {e prior} to running any computation.
    [pre build] will run on top of [build]. *)

val post : ?diff:Diff.t -> ?tracelog:Tracelog.t -> int64 -> post
(** Constructs information {e about} the execution (like the time taken for
    example). *)

val v : ?overlays:Zfs_store.Build.t list -> pre -> post -> entry
(** A new entry with optional overlays. *)

val with_pre :
  ?mode:mode ->
  ?args:string list ->
  ?env:string list ->
  ?cwd:string ->
  ?user:int * int ->
  ?shell:string ->
  ?build:Zfs_store.Build.t ->
  pre ->
  pre
(** A utility function for construction a {! pre} from another {! pre}. *)

val with_post :
  ?diff:Diff.t -> ?tracelog:Tracelog.t -> ?time:int64 -> post -> post
(** A utility function for construction a {! post} from another {! post}. *)

val latest : t -> entry
(** Returns the latest entry in a history *)

val empty : t
(** The empty history *)

val pp : t Fmt.t

include Irmin.Contents.S with type t := t
