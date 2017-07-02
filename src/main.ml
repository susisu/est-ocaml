open Core

let spec =
  let open Command.Spec in
  empty
  +> anon ("program" %: string)

let parse_program prog =
  let lexbuf = Lexing.from_string prog in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = "" };
  let open Parser.MenhirInterpreter in
  try loop_handle
    (fun term -> Ok term)
    (function
      | HandlingError e ->
        let (p, _) = positions e in
        let s = current_state_number e in
        let msg =
          try
            let m = String.drop_suffix (Parser_messages.message s) 1 in
            sprintf "Parse Error at %s:\n  %s" (Common.Position.to_string p) m
          with
          | Not_found -> "Parse Error"
        in
        Error msg
      | _ -> Error "Parse Error"
    )
    (lexer_lexbuf_to_supplier Lexer.main lexbuf)
    (Parser.Incremental.toplevel lexbuf.Lexing.lex_curr_p)
  with
  | Lexer.Error msg -> Error msg

let main prog () =
  match parse_program prog with
  | Error msg -> print_endline msg; exit 1
  | Ok term -> Term.to_string term |> print_endline

let command = Command.basic spec main
    ~summary:"Simple calculator"

let () = Command.run command
