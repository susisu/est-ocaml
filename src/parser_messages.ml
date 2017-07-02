
(* This file was auto-generated based on "src/parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 43 ->
        "expecting number, vector, identifier, '(', operator or ')'\n"
    | 40 ->
        "expecting number, vector, identifier, '(', operator or 'in'\n"
    | 12 ->
        "expecting number, vector, identifier, '(', operator or ','\n"
    | 48 | 24 ->
        "expecting number, vector, identifier, '(' or operator\n"
    | 1 | 3 | 28 | 34 | 16 | 32 | 30 | 18 | 26 | 20 ->
        "expecting number, vector, identifier or '('\n"
    | 6 ->
        "expecting '='\n"
    | 5 ->
        "expecting identifier\n"
    | 0 | 4 | 8 | 13 | 7 | 41 ->
        "expecting term\n"
    | _ ->
        raise Not_found
