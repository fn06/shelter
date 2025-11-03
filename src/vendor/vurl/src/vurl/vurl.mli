type t
(** A versioned URL *)

val pp : Format.formatter -> t -> unit
(** A pretty printer for Vurls. *)

val to_string : ?minify:bool -> t -> string
(** Serialises a Vurl to their canonical format *)

val of_string_exn : string -> t
(** Deserialises a Vurl.

    @raises Invalid_argument if malformatted. *)

val intentional_uri : t -> Uri.t option
(** The original URI that specified the intent for a Vurl *)

type segment = { uri : Uri.t; cid : Cid.t }

val decapsulate : t -> [ `Segment of segment * t | `URI of Uri.t ]
(** Removes a layer of the Vurl. If only the intentional URI is left
    then returns [`URI uri]. *)

val next_uri : t -> Uri.t
(** The next URI that might need resolved *)

val encapsulate : t -> Cid.t -> Uri.t -> t
(** Adds another layer of precision to an existing Vurl. *)

val of_uri : string -> t
(** Makes a new Vurl from an intentional URI. *)

val cid : ?codec:Multicodec.t -> Cstruct.t -> Cid.t
(** Cid generator for Vurl's *)

module Resource = Resource
module Rpc = Rpc

val add_resolver : Rpc.t -> unit
val file : t -> t * Resource.File.t
val ptr : t -> t * Resource.Ptr.t
val git : t -> t * Resource.Git.t
