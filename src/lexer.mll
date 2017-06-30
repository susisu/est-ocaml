{
open Core
open Lexing

open Parser

let reserved_ids = String.Map.of_alist_reduce ~f:(fun _ x -> x) [
    ("let", fun p -> LET p);
    ("in",  fun p -> IN p);
  ]

let reserved_ops = String.Map.of_alist_reduce ~f:(fun _ x -> x) [
    ("+",  fun p -> PLUS p);
    ("-",  fun p -> MINUS p);
    ("*",  fun p -> TIMES p);
    ("/",  fun p -> FRAC p);
    ("%",  fun p -> MOD p);
    ("**", fun p -> POWER p);
    ("^",  fun p -> CARET p);
    ("!",  fun p -> EXCL p);
    ("=",  fun p -> EQUAL p);
  ]

exception Error of string
}

let whitespace = [' ' '\r' '\t']
let newline    = '\n'
let digit       = ['0'-'9']
let letter      = ['A'-'Z' 'a'-'z']
let alphaNum    = letter | digit

let fraction = '.' digit*
let exponent = ['E' 'e'] ['+' '-']? digit+
let float    = digit+ fraction? exponent?

let identifier = letter (alphaNum | ['_' ','])*

let operator = ['+' '-' '*' '/' '%' '^' '!' '=']+

rule main = parse
  | whitespace { main lexbuf }
  | newline    { new_line lexbuf; main lexbuf }
  | float      { NUM (lexbuf.lex_start_p, Float.of_string (lexeme lexbuf)) }
  | identifier {
      let name = lexeme lexbuf in
      match Map.find reserved_ids name with
      | Some f -> f (lexbuf.lex_start_p)
      | None -> ID (lexbuf.lex_start_p, name)
    }
  | operator {
      let name = lexeme lexbuf in
      match Map.find reserved_ops name with
      | Some f -> f (lexbuf.lex_start_p)
      | None -> raise (Error ("  unknown operator: " ^ name))
    }
  | '[' { LBRACKET (lexbuf.lex_start_p) }
  | ']' { RBRACKET (lexbuf.lex_start_p) }
  | ',' { COMMA (lexbuf.lex_start_p) }
  | '(' { LPAREN (lexbuf.lex_start_p) }
  | ')' { RPAREN (lexbuf.lex_start_p) }
  | _   { raise (Error ("  unexpected character: " ^ lexeme lexbuf)) }
  | eof { EOF (lexbuf.lex_start_p) }
