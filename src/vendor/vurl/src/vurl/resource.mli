module File : sig
  type t = { path : string }
end

module Git : sig
  type t = { path : string }
end

module Ptr : sig
  type t =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
end

module Error : sig
  type t = { description : string }
end

type t = Rpc.Resource_16038180360818139020.t =
  | File
  | Git
  | Ptr
  | Unit
  | Error
  | Undefined of int
