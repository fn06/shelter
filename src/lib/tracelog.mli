type entry [@@derivign repr]
(** A trace entry *)

type t = entry list [@@deriving repr]
(** A trace log from bpftrace *)

val empty : t
(** The empty log *)

val of_bpftrace : string -> t
(** Takes a raw bpftrace output (including extra lines) and converts to a log.
*)

val has_flag : entry -> Flags.t -> bool
(** Check if an open entry has flags. *)

val pp : t Fmt.t
(** Pretty printer for entries. *)

val reads : t -> string list
(** All the files that might have been read. *)

val writes : t -> string list
(** All the files that might have been written to. *)
