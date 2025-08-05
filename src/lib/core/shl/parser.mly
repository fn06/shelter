%{
  open Ast
%}

%token LPAREN RPAREN
%token LCURLY RCURLY
%token IF THEN ELSE
%token FOR IN
%token NEWLINE
%token DOLLAR
%token SEMICOLON
%token <string> WORD
%token <string> LINE
%token EOF

%start <Ast.t> expr 
%%

single_expr:
  | FOR; for_ = WORD; IN; in_ = WORD; LCURLY; NEWLINE; b = body              { make_for ~for_ ~in_ b }
  | IF; if_ = WORD; THEN; LCURLY; then_ = body; ELSE; LCURLY; else_ = body   { make_if_then_else ~if_ ~then_ ~else_ }
  | action = line { make_action action }

line:
  | { "" }
  | w = WORD; ws = line { w ^ " " ^ ws }

body:
  | RCURLY { [ ] }
  | e = single_expr; NEWLINE; es = body { e :: es }

expr:
  | EOF { [ ] }
  | e = single_expr; NEWLINE; es = expr { e :: es }

