type iterable = Directory of string | Glob of string | List of string list
type command = string

type expression =
  | Action of command
  | For of { for_ : string; in_ : iterable; body : expression list }
  | If_then_else of {
      if_ : string;
      then_ : expression list;
      else_ : expression list;
    }
  | Newline

type t = expression list

let pp_iterable fmt = function
  | Directory s -> Fmt.string fmt s
  | Glob s -> Fmt.string fmt s
  | List s -> Fmt.pf fmt "[ %a ]" Fmt.(list ~sep:(Fmt.any ", ") string) s

let rec pp_expression fmt = function
  | Action s -> Format.fprintf fmt "%s" (String.trim s)
  | For { for_; in_; body } ->
      Fmt.pf fmt "@[<v2>for %s in %a {@,%a@]@,}" for_ pp_iterable in_
        (Fmt.list pp_expression) body
  | If_then_else { if_; then_; else_ } ->
      Fmt.pf fmt "@[<v2>if %s then {%a@]@,@[<v2>} else {%a@]@,}"
        (String.trim if_) (Fmt.list pp_expression) then_
        (Fmt.list pp_expression) else_
  | Newline -> ()

let pp fmt vs = Fmt.pf fmt "@[<v>%a@]@." (Fmt.list pp_expression) vs
let make_for ~for_ ~in_ body = For { for_; in_; body }
let make_action w = Action w
let make_if_then_else ~if_ ~then_ ~else_ = If_then_else { if_; then_; else_ }
