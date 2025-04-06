module Store = Store

module History : sig
  type t = {
    mode : Void.mode;
    build : Store.Build.t;
    args : string list;
    time : int64;
    env : string list;
    cwd : string;
    diff : Diff.t;
  }
  [@@deriving repr]

  include Irmin.Contents.S with type t := t
end

include Shelter.Engine.S with type entry = History.t
