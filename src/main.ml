open Core

let spec =
  let open Command.Spec in
  empty
  +> anon ("program" %: string)
  +> anon ("file" %: file)

let die msg =
  Out_channel.output_string Out_channel.stderr (msg ^ "\n");
  exit 1

let parse_program prog =
  let lexbuf = Lexing.from_string prog in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = "" };
  let open Parser.MenhirInterpreter in
  let succeed term = term in
  let fail = function
    | HandlingError e ->
      let (p, _) = positions e in
      let p_str = Common.Position.to_string p in
      let s = current_state_number e in
      let msg =
        try String.drop_suffix (Parser_messages.message s) 1 with
        | Not_found -> ""
      in
      let msg' =
        if msg = "" then sprintf "Parse Error at %s" p_str
        else sprintf "Parse Error at %s:\n  %s" p_str msg
      in
      die msg'
    | _ -> die "Parse Error"
  in
  let suppiler = lexer_lexbuf_to_supplier Lexer.main lexbuf in
  let checkpoint = Parser.Incremental.toplevel lexbuf.Lexing.lex_curr_p in
  try loop_handle succeed fail suppiler checkpoint with
  | Lexer.Error msg -> die msg

let read_data file =
  let open Reader in
  let module R = Make_reader(Table_extended) in
  In_channel.with_file file ~f:(fun ch ->
      R.read_from_channel { id = 0; transpose = false } ch
    )

let eval_term ctx term =
  let module E = Eval.Make_eval(Common.Position) in
  try E.eval ctx term with
  | Eval.Runtime_error msg -> die msg

let main prog file () =
  let term = parse_program prog in
  let ctx = read_data file in
  let ctx' = Eval.Context.append Lib.std ctx in
  let v = eval_term ctx' term in
  Value.to_string v |> print_endline

let command = Command.basic spec main
    ~summary:"Simple calculator"

let () = Command.run command
