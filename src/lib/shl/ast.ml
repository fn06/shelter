type expression =
  | Action of string
  | For of { for_ : string; in_ : string; body : expression list }
  | If_then_else of {
      if_ : string;
      then_ : expression list;
      else_ : expression list;
    }
  | Newline

type t = expression list

let rec pp_expression fmt = function
  | Action s -> Format.fprintf fmt "%s" (String.trim s)
  | For { for_; in_; body } ->
      Fmt.pf fmt "@[<v2>for %s in %s {@,%a@]@,}" for_ in_
        (Fmt.list pp_expression) body
  | If_then_else { if_; then_; else_ } ->
      Fmt.pf fmt "@[<v2>if %s then {%a@]@,@[<v2>} else {%a@]@,}" if_
        (Fmt.list pp_expression) then_ (Fmt.list pp_expression) else_
  | Newline -> ()

let pp fmt vs = Fmt.pf fmt "@[<v>%a@]@." (Fmt.list pp_expression) vs
let make_for ~for_ ~in_ body = For { for_; in_; body }
let make_action w = Action w
let make_if_then_else ~if_ ~then_ ~else_ = If_then_else { if_; then_; else_ }
