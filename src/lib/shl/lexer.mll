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
  | EOF -> print_endline "EOF"
  | WORD s -> Printf.printf "WORD(%s)\n" s
  | LINE s -> Printf.printf "LINE(%s)\n" s
  | DOLLAR -> print_endline "DOLLAR"
  | NEWLINE -> print_endline "NEWLINE"
  | SEMICOLON -> print_endline "SEMICOLON"
}

let white = [' ' '\t']+
let word = ['a'-'z' 'A'-'Z' '0'-'9' '_' '.' '*' '/' '<' '>']+
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
  | ';'      { SEMICOLON }
  | "for"    { FOR }
  | "in"     { IN }
  | "if"     { IF }
  | "then"   { THEN }
  | "else"   { ELSE }
  | ')'      { RPAREN }
  | '('      { LPAREN }
  | '{'      { LCURLY }
  | '}'      { RCURLY }
  | word as w { WORD w }
  | eof      { EOF }
  (* | _        { read_line (Lexing.lexeme lexbuf) lexbuf } *)
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

(* and read_line c = parse *)
(*   | line as l { LINE (c ^ l) } *)
