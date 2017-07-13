open Core

let die msg =
  Out_channel.output_string Out_channel.stderr (msg ^ "\n");
  exit 1

let die_error msg detail = die (Common.format_error_message msg detail)


let parse_program prog =
  let lexbuf = Lexing.from_string prog in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = "" };
  let open Parser.MenhirInterpreter in
  let succeed term = term in
  let fail = function
    | HandlingError env ->
      let pos = positions env |> fst |> Common.Position.to_string in
      let state = current_state_number env in
      let msg = try String.drop_suffix (Parser_messages.message state) 1 with
        | Not_found -> ""
      in
      die_error ("parse error at " ^ pos) msg
    | _ -> die "parse error"
  in
  let suppiler = lexer_lexbuf_to_supplier Lexer.main lexbuf in
  let checkpoint = Parser.Incremental.toplevel lexbuf.Lexing.lex_curr_p in
  try loop_handle succeed fail suppiler checkpoint with
  | Lexer.Lex_error (pos, msg) -> die_error ("parse error at" ^ pos) msg

let read_data read_from_channel id file =
  try match file with
    | "-" -> read_from_channel id In_channel.stdin
    | _ -> In_channel.with_file file ~f:(read_from_channel id)
  with
  | Sys_error msg -> die msg
  | Reader.Read_error msg -> die_error "read error" msg

let eval_term ctx term =
  let module E = Eval.Make_eval(Common.Position) in
  try E.eval ctx term with
  | Eval.Runtime_error (Some pos, msg) -> die_error ("runtime error at " ^ pos) msg
  | Eval.Runtime_error (None, msg) -> die_error "runtime error" msg

let print_value print_to_channel value =
  try print_to_channel Out_channel.stdout value with
  | Printer.Print_error msg -> die_error "print error" msg


module type Reader_entry = sig
  module R : Reader.Reader_intf
  val get_options : Config.Reader_config.options -> R.Config.options
  val default_config : R.Config.t
end

let readers =
  let open Reader in
  let open Config.Reader_config in
  String.Map.of_alist_reduce ~f:(fun _ x -> x) [
    ("table",
     (module struct
       module R = Table
       let get_options config = config.table
       let default_config = R.Config.({
           strict    = true;
           separator = [' '; '\t'];
           default   = Float.nan;
           transpose = false;
         })
     end : Reader_entry));
    ("table_ex",
     (module struct
       module R = Table_ex
       let get_options config = config.table_ex
       let default_config = R.Config.({
           strict    = true;
           separator = [' '; '\t'];
           default   = Float.nan;
           transpose = false;
         })
     end : Reader_entry));
  ]

let create_read_from_channel config name opts_sexp =
  let open Config in
  let resolved_name = Option.value ~default:"table"
      (Option.first_some name config.reader)
  in
  match Map.find readers resolved_name with
  | None -> die (
      "unknown reader name: " ^ resolved_name ^ "\n" ^
      "see -list-readers for the available reader names"
    )
  | Some entry ->
    let module Entry = (val entry) in
    let module R = Entry.R in
    let opts = match opts_sexp with
      | None -> Entry.get_options config.reader_options
      | Some s ->
        let opts' = try R.Config.options_of_sexp s with
          | e -> die ("error on reading reader option:\n" ^ Exn.to_string e)
        in
        R.Config.merge_options (Entry.get_options config.reader_options) opts'
    in
    R.read_from_channel (R.Config.of_options opts ~default:Entry.default_config)


module type Printer_entry = sig
  module P : Printer.Printer_intf
  val get_options : Config.Printer_config.options -> P.Config.options
  val default_config : P.Config.t
end

let printers =
  let open Printer in
  let open Config.Printer_config in
  String.Map.of_alist_reduce ~f:(fun _ x -> x) [
    ("table",
     (module struct
       module P = Table
       let get_options config = config.table
       let default_config = P.Config.({
           strict    = true;
           separator = "\t";
           precision = 8;
           default   = Float.nan;
           transpose = false;
         })
     end : Printer_entry));
  ]

let create_print_to_channel config name opts_sexp =
  let open Config in
  let resolved_name = Option.value ~default:"table"
      (Option.first_some name config.printer)
  in
  match Map.find printers resolved_name with
  | None -> die (
      "unknown printer name: " ^ resolved_name ^ "\n" ^
      "see -list-printers for the available printer names"
    )
  | Some entry ->
    let module Entry = (val entry) in
    let module P = Entry.P in
    let opts = match opts_sexp with
      | None -> Entry.get_options config.printer_options
      | Some s ->
        let opts' = try P.Config.options_of_sexp s with
          | e -> die ("error on reading printer option:\n" ^ Exn.to_string e)
        in
        P.Config.merge_options (Entry.get_options config.printer_options) opts'
    in
    P.print_to_channel (P.Config.of_options opts ~default:Entry.default_config)


let print_list list = List.iter list ~f:(fun item -> print_endline item)
let list_readers () = Map.keys readers |> print_list
let list_printers () = Map.keys printers |> print_list


let read_config file =
  try Sexp.load_sexp_conv_exn file Config.t_of_sexp with
  | Sys_error _ -> Config.default
  | e -> die ("error on reading config file \"" ^ file ^ "\":\n" ^ (Exn.to_string e))

let main r_name r_opts p_name p_opts prog files () =
  let config_file = Filename.concat (Sys.home_directory ()) ".estconfig" in
  let config = read_config config_file in
  let read_from_channel = create_read_from_channel config r_name r_opts in
  let print_to_channel = create_print_to_channel config p_name p_opts in
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
  +> flag "-reader" (optional string)
    ~doc:"NAME specify reader"
    ~aliases:["-r"]
  +> flag "-reader-options" (optional sexp)
    ~doc:"SEXP specify reader options"
    ~aliases:["-ro"]
  +> flag "-printer" (optional string)
    ~doc:"NAME specify printer"
    ~aliases:["-p"]
  +> flag "-printer-options" (optional sexp)
    ~doc:"SEXP specify printer options"
    ~aliases:["-po"]
  +> anon ("program" %: string)
  +> anon (sequence ("files" %: file))

let command = Command.basic spec main
    ~summary:"est: Simple vector calculator"

let () = Command.run command
    ~version:"0.1.0"
    ~build_info:""
