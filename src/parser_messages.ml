
(* This file was auto-generated based on "src/parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 45 ->
        "expecting number, vector, identifier, '(', operator or ')'\n"
    | 42 ->
        "expecting number, vector, identifier, '(', operator or 'in'\n"
    | 12 ->
        "expecting number, vector, identifier, '(', operator, ',' or ']'\n"
    | 50 | 23 | 27 ->
        "expecting number, vector, identifier, '(' or operator\n"
    | 1 | 3 | 30 | 36 | 16 | 34 | 32 | 18 | 28 | 20 | 24 ->
        "expecting number, vector, identifier or '('\n"
    | 6 ->
        "expecting '='\n"
    | 5 ->
        "expecting identifier\n"
    | 0 | 4 | 8 | 13 | 7 | 43 ->
        "expecting term\n"
    | _ ->
        raise Not_found
