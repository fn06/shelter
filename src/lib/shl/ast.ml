type iterable = Directory of string | Glob of string | List of string list
type 'a loc = { v : 'a; loc : Lexing.position * Lexing.position }

let dummy_pos = (Lexing.dummy_pos, Lexing.dummy_pos)
let with_loc start_pos end_pos v = { v; loc = (start_pos, end_pos) }
let value loc = loc.v

type command = string loc
type meta = string loc

type for_ = { for_ : string; in_ : iterable; body : expression list }
and if_ = { if_ : command; then_ : expression list; else_ : expression list }

and expression =
  | Action of command
  | Meta of meta
  | For of for_ loc
  | If_then_else of if_ loc
  | Newline

type t = expression list

let pp_iterable fmt = function
  | Directory s -> Fmt.string fmt s
  | Glob s -> Fmt.string fmt s
  | List s -> Fmt.pf fmt "[ %a ]" Fmt.(list ~sep:(Fmt.any ", ") string) s

let rec pp_expression fmt = function
  | Action s -> Format.fprintf fmt "%s" (String.trim @@ value s)
  | Meta s -> Format.fprintf fmt "%@ %s" (String.trim @@ value s)
  | For { v = { for_; in_; body }; _ } ->
      Fmt.pf fmt "@[<v2>for %s in %a {@,%a@]@,}" for_ pp_iterable in_
        (Fmt.list pp_expression) body
  | If_then_else { v = { if_; then_; else_ }; _ } ->
      Fmt.pf fmt "@[<v2>if %s then {%a@]@,@[<v2>} else {%a@]@,}"
        (String.trim @@ value if_)
        (Fmt.list pp_expression) then_ (Fmt.list pp_expression) else_
  | Newline -> ()

let pp fmt vs = Fmt.pf fmt "@[<v>%a@]@." (Fmt.list pp_expression) vs

let make_for ?(loc = dummy_pos) ~for_ ~in_ body =
  For { v = { for_; in_; body }; loc }

let make_action ?(loc = dummy_pos) w = Action { v = w; loc }
let make_meta ?(loc = dummy_pos) w = Meta { v = w; loc }

let make_if_then_else ?(loc = dummy_pos) ~if_ ~then_ ~else_ () =
  If_then_else { v = { if_; then_; else_ }; loc }
