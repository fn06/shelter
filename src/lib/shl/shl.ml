module Ast = Ast

type src = [ `In_channel of in_channel | `String of string ]

let with_debug lexbuf =
  let tok = Lexer.read lexbuf in
  (* Lexer.print_token tok; *)
  tok

let print_position outx lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Format.fprintf outx "%s:%d:%d"
    (if "" = pos.pos_fname then "stdin" else pos.pos_fname)
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let of_src ?filename src =
  let lexbuf =
    match src with
    | `In_channel ic -> Lexing.from_channel ic
    | `String s -> Lexing.from_string s
  in
  Option.iter (Lexing.set_filename lexbuf) filename;
  try Parser.expr with_debug lexbuf
  with Parser.Error ->
    Format.eprintf "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let format src = of_src src |> Ast.pp Format.std_formatter
