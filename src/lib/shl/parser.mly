%{
  open Ast

  let parse_iterable s =
    if String.contains s '*' then Glob s else Directory s
%}

%token LPAREN RPAREN
%token LCURLY RCURLY
%token LSQUARE RSQUARE
%token COMMA
%token IF THEN ELSE
%token FOR IN
%token NEWLINE
%token <string> WORD
%token <string> LINE
%token EOF

%start <Ast.t> expr 
%%


command:
  | l = line { l }

single_expr:
  | FOR; for_ = WORD; IN; in_ = iterable; LCURLY; NEWLINE; b = body              { make_for ~for_ ~in_ b }
  | IF; if_ = command; THEN; LCURLY; then_ = body; ELSE; LCURLY; else_ = body   { make_if_then_else ~if_ ~then_ ~else_ }
  | action = command { make_action action }

line:
  | { "" }
  | w = WORD; ws = line { w ^ " " ^ ws }

body:
  | RCURLY { [ ] }
  | e = single_expr; NEWLINE; es = body { e :: es }

expr:
  | EOF { [ ] }
  | e = single_expr; NEWLINE; es = expr { e :: es }

iterable:
  | LSQUARE; s = separated_list(COMMA, WORD); RSQUARE { List s }
  | w = WORD { parse_iterable w }

