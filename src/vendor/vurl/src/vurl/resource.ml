module File = struct
  type t = { path : string }
end

module Git = struct
  type t = { path : string }
end

module Ptr = struct
  type t =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
end

module Error = struct
  type t = { description : string }
end

type t = Rpc.Resource_16038180360818139020.t =
  | File
  | Git
  | Ptr
  | Unit
  | Error
  | Undefined of int
