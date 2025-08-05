module Store = Store
module History = History

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
  (* Merging *)
  | Merge of string
  (* Display info *)
  | Info of [ `Current | `History ]
  (* Error state *)
  | Unknown of string list
[@@deriving repr]

include
  Shelter.Engine.S
    with type contents = History.t
     and type store = Store.t
     and type action := action
