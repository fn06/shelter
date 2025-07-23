module Store = Store

module History : sig
  type mode = Void.mode
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

type action =
  (* Change modes *)
  | Set_mode of History.mode
  (* Fork a new branch from an existing one,
     or switch to a branch if it exists *)
  | Set_session of string
  (* Run a command *)
  | Exec of string list
  (* Undo the last command *)
  | Undo
  (* Replay the current branch onto another *)
  | Replay of string
  (* Display info *)
  | Info of [ `Current | `History ]
  (* Error state *)
  | Unknown of string list
[@@deriving repr]

include Shelter.Engine.S with type entry = History.t and type action := action
