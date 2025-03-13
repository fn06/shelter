module Build : sig
  type t = Image of string | Build of Cid.t [@@deriving repr]
end

type mode = R | RW

module History : sig
  type t = { mode : mode; build : Build.t; args : string list }
  [@@deriving repr]

  include Irmin.Contents.S with type t := t
end

include Cshell.Engine.S with type entry = History.t
