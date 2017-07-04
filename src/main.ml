open Core

let spec =
  let open Command.Spec in
  empty
  +> flag "-reader" (optional_with_default "table" string)
    ~doc:"NAME specify reader (default: table)"
  +> flag "-reader-options" (optional string)
    ~doc:"SEXP specify reader options as an S-expression"
  +> anon ("program" %: string)
  +> anon (sequence ("files" %: file))


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


let read_data read_from_channel id file =
  match file with
  | "-" -> read_from_channel id In_channel.stdin
  | _ -> try In_channel.with_file file ~f:(read_from_channel id) with
    | Sys_error msg -> die msg


let eval_term ctx term =
  let module E = Eval.Make_eval(Common.Position) in
  try E.eval ctx term with
  | Eval.Runtime_error msg -> die msg


let readers =
  let open Reader in
  String.Map.of_alist_reduce ~f:(fun _ x -> x) [
    ("table",    (module Table : Reader));
    ("table_ex", (module Table_extended : Reader))
  ]

let create_read_from_channel name opts =
  match Map.find readers name with
  | Some reader ->
    let module R = (val reader) in
    let opts' = match opts with
      | None -> R.default_options
      | Some s -> try Sexp.of_string s |> R.options_of_sexp with
        | Failure _ | Sexplib.Conv.Of_sexp_error _ -> die ("Ill-formed reader option: " ^ s)
    in
    R.read_from_channel opts'
  | None -> die ("Unknown reader name: " ^ name)

let main reader_name reader_opts prog files () =
  let read_from_channel = create_read_from_channel reader_name reader_opts in
  let term = parse_program prog in
  let ctxs = List.mapi files ~f:(read_data read_from_channel) in
  let ctx = List.fold_left ctxs ~init:Lib.std ~f:Eval.Context.append in
  let v = eval_term ctx term in
  Value.to_string v |> print_endline

let command = Command.basic spec main
    ~summary:"Simple calculator"

let () = Command.run command
