module type S = sig
  type t
  (** A single history entry *)

  include Irmin.Contents.S with type t := t
end

type 'entry t =
  | Store :
      ((module Irmin.S
          with type t = 'a
           and type Schema.Branch.t = string
           and type Schema.Contents.t = 'entry
           and type Schema.Path.t = string list
           and type Schema.Path.step = string)
      * 'a)
      -> 'entry t
