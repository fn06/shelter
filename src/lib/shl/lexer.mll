{
open Parser
exception SyntaxError of string

let print_token = function
  | FOR -> print_endline "FOR"
  | IN -> print_endline "IN"
  | IF -> print_endline "IF"
  | THEN -> print_endline "THEN"
  | ELSE -> print_endline "ELSE"
  | LPAREN -> print_endline "LPAREN"
  | RPAREN -> print_endline "RPAREN"
  | LCURLY -> print_endline "LCURLY"
  | RCURLY -> print_endline "RCURLY"
  | LSQUARE -> print_endline "LSQUARE"
  | RSQUARE -> print_endline "RSQUARE"
  | COMMA -> print_endline "COMMA"
  | EOF -> print_endline "EOF"
  | WORD s -> Printf.printf "WORD(%s)\n" s
  | LINE s -> Printf.printf "LINE(%s)\n" s
  | NEWLINE -> print_endline "NEWLINE"
}

let white = [' ' '\t']+
let word = ['a'-'z' 'A'-'Z' '0'-'9' '_' '.' '*' '/' '<' '>' '"' '-' '$' '{' '}']+
let line = (word | white)+
let newline = '\r' | '\n' | "\r\n"

rule read =
  parse
  | '\\' newline {
    (* New line continuation *)
    Lexing.new_line lexbuf;
    read lexbuf
  }
  | newline  { Lexing.new_line lexbuf; NEWLINE }
  | white    { read lexbuf }
  | "for"    { FOR }
  | "in"     { IN }
  | "if"     { IF }
  | "then"   { THEN }
  | "else"   { ELSE }
  | ')'      { RPAREN }
  | '('      { LPAREN }
  | '{'      { LCURLY }
  | '}'      { RCURLY }
  | ']'      { RSQUARE }
  | '['      { LSQUARE }
  | ','      { COMMA }
  | word as w { WORD w }
  | eof      { EOF }
  (* | _        { read_line (Lexing.lexeme lexbuf) lexbuf } *)
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

(* and read_line c = parse *)
(*   | line as l { LINE (c ^ l) } *)
