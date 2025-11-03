open Eio

module File : sig
  type t
  (** A resolver that targets files. *)

  val directory : t -> Eio.Fs.dir_ty Eio.Path.t
  (** The directory where all the files are stored. *)
end

(** {1 Vurl Interfaces} *)

val of_file : _ Path.t -> Vurl.Resource.File.t -> File.t

(** {1 Resolver Services} *)

val doi : _ Eio.Net.t -> Vurl_resolver.middleware
(** Handles intermediate DOI resolution *)

val file_resolver :
  ?name:(Uri.t -> string) ->
  ?progress:((string * int) option -> int Progress.Line.t) ->
  _ Eio.Net.t ->
  _ Eio.Path.t ->
  Vurl_resolver.handler
(** A new file resolver for a particular directory. [name] is used to convert a
    URI to a file name and [progress] can be used to optionally report progress
    made by the download. *)

val git_resolver :
  ?name:(Uri.t -> string) -> _ Eio.Path.t -> Vurl_resolver.handler

val run :
  ?resolve_uri:Uri.t Eio.Promise.u ->
  secret_key:[< `Ephemeral | `File of _ Eio.Path.t | `PEM of string ] ->
  sw:Eio.Switch.t ->
  listen_address:Capnp_rpc_unix.Network.Location.t ->
  net:_ Eio.Net.t ->
  Vurl_resolver.handler ->
  Uri.t
(** Run a resolver service *)

(** {1 Clients} *)

val connect_exn :
  sw:Switch.t -> _ Net.t -> Uri.t -> 'a Capnp_rpc.Capability.t
(** A client-only connection to a capability *)

val with_cap : net:_ Net.t -> _ Path.t -> (unit -> 'a) -> 'a
(** Simple helper to set up a capability to a resolver from a cap URI in a file.*)

val with_default : net:_ Net.t -> _ Path.t -> (unit -> 'a) -> 'a
(** [with_default ~net path fn] runs [fn] with the default resolver installed. *)
