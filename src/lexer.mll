{
open Core
open Lexing

open Parser

let reserved_ids = String.Map.of_alist_reduce ~f:(fun _ x -> x) [
    ("let", fun () -> LET);
    ("in",  fun () -> IN);
  ]

let reserved_ops = String.Map.of_alist_reduce ~f:(fun _ x -> x) [
    ("+",  fun () -> PLUS);
    ("-",  fun () -> MINUS);
    ("*",  fun () -> TIMES);
    ("/",  fun () -> FRAC);
    ("%",  fun () -> MOD);
    ("**", fun () -> POWER);
    ("^",  fun () -> CARET);
    ("!",  fun () -> EXCL);
    ("=",  fun () -> EQUAL);
  ]

exception Error of string
}

let white_space = [' ' '\r' '\t']
let new_line    = '\n'
let digit       = ['0'-'9']
let letter      = ['A'-'Z' 'a'-'z']
let alphaNum    = letter | digit

let fraction = '.' digit*
let exponent = ['E' 'e'] ['+' '-']? digit+
let float    = digit+ fraction? exponent?

let identifier = letter (alphaNum | ['_' ','])*

let operator = ['+' '-' '*' '/' '%' '^' '!' '=']+

rule main = parse
  | white_space { main lexbuf }
  | new_line    { Lexing.new_line lexbuf; main lexbuf }
  | float       { NUM (Float.of_string (Lexing.lexeme lexbuf)) }
  | identifier  {
      let name = Lexing.lexeme lexbuf in
      match Map.find reserved_ids name with
      | Some f -> f ()
      | None -> ID name
    }
  | operator {
      let name = Lexing.lexeme lexbuf in
      match Map.find reserved_ops name with
      | Some f -> f ()
      | None -> raise (Error ("  unknown operator: " ^ name))
    }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | ',' { COMMA }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | _   { raise (Error ("  unexpected character: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
