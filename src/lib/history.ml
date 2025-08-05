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
