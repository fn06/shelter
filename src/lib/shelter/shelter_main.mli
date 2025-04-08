module Store = Store

module History : sig
  type post = { diff : Diff.t; time : int64 } [@@deriving repr]

  type pre = {
    mode : Void.mode;
    build : Store.Build.t;
    args : string list;
    env : string list;
    cwd : string;
    user : int * int;
  }
  [@@deriving repr]
  (** Needed for execution *)

  type t = { pre : pre; post : post } [@@deriving repr]

  include Irmin.Contents.S with type t := t
end

include Shelter.Engine.S with type entry = History.t
