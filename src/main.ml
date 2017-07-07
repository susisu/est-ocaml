open Core

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
        if msg = "" then sprintf "parse error at %s" p_str
        else sprintf "parse error at %s:\n  %s" p_str msg
      in
      die msg'
    | _ -> die "parse error"
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


let print_value print_to_channel value =
  try print_to_channel Out_channel.stdout value with
  | Printer.Ill_formed_output msg -> die ("ill-formed output: " ^ msg)


let readers =
  let open Reader in
  String.Map.of_alist_reduce ~f:(fun _ x -> x) [
    ("table",    (module Table : Reader));
    ("table_ex", (module Table_extended : Reader))
  ]

let create_read_from_channel name opts =
  match Map.find readers name with
  | None -> die (
      "unknown reader name: " ^ name ^ "\n" ^
      "see -list-readers for the available reader names"
    )
  | Some reader ->
    let module R = (val reader) in
    let opts' = match opts with
      | None -> R.default_options
      | Some s -> try R.options_of_sexp s with
        | e -> die ("ill-formed reader option:\n" ^ Exn.to_string e)
    in
    R.read_from_channel opts'

let printers =
  let open Printer in
  String.Map.of_alist_reduce ~f:(fun _ x -> x) [
    ("table", (module Table : Printer));
  ]

let create_print_to_channel name opts =
  match Map.find printers name with
  | None -> die (
      "unknown printer name: " ^ name ^ "\n" ^
      "see -list-printers for the available printer names"
    )
  | Some printer ->
    let module P = (val printer) in
    let opts' = match opts with
      | None -> P.default_options
      | Some s -> try P.options_of_sexp s with
        | e -> die ("ill-formed printer option:\n" ^ Exn.to_string e)
    in
    P.print_to_channel opts'

let print_list list = List.iter list ~f:(fun item -> print_endline item)

let list_readers () = Map.keys readers |> print_list

let list_printers () = Map.keys printers |> print_list

let main r_name r_opts p_name p_opts prog files () =
  let read_from_channel = create_read_from_channel r_name r_opts in
  let print_to_channel = create_print_to_channel p_name p_opts in
  let ctx = List.mapi files ~f:(read_data read_from_channel)
            |> List.fold_left ~init:Lib.std ~f:Eval.Context.append in
  parse_program prog
  |> eval_term ctx
  |> print_value print_to_channel


let spec =
  let open Command.Spec in
  step (fun m () () r_name r_opts p_name p_opts prog files ->
      m r_name r_opts p_name p_opts prog files
    )
  +> flag "-list-readers" (no_arg_abort ~exit:(fun () -> list_readers (); exit 0))
    ~doc:"print list of the available readers"
    ~aliases:["-ls-r"; "-ls-readers"]
  +> flag "-list-printers" (no_arg_abort ~exit:(fun () -> list_printers (); exit 0))
    ~doc:"print list of the available printers"
    ~aliases:["-ls-p"; "-ls-printers"]
  +> flag "-reader" (optional_with_default "table" string)
    ~doc:"NAME specify reader (default: table)"
    ~aliases:["-r"]
  +> flag "-reader-options" (optional sexp)
    ~doc:"SEXP specify reader options"
    ~aliases:["-ro"]
  +> flag "-printer" (optional_with_default "table" string)
    ~doc:"NAME specify printer (default: table)"
    ~aliases:["-p"]
  +> flag "-printer-options" (optional sexp)
    ~doc:"SEXP specify printer options"
    ~aliases:["-po"]
  +> anon ("program" %: string)
  +> anon (sequence ("files" %: file))

let command = Command.basic spec main
    ~summary:"Simple calculator"

let () = Command.run command
