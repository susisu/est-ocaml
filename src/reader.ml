open Core

module type Reader_intf = sig
  type options
  val default_options : options
  val options_of_sexp : Sexplib.Sexp.t -> options
  val read_from_channel : options -> int -> In_channel.t -> Eval.Context.t
end


let split_words sep str = String.split_on_chars ~on:sep str
                      |> List.filter ~f:(fun word -> word <> "")

let read_num str =
  let num = try Float.of_string str with
    | Invalid_argument _ -> Float.nan
  in
  Value.Num num

let read_line sep line = split_words sep line |> List.map ~f:read_num

let create_table sep lines =
  let lines = List.map lines ~f:(read_line sep) in
  let width = List.fold lines ~init:0 ~f:(fun w line -> Int.max w (List.length line)) in
  List.map lines ~f:(fun line ->
      let row = Array.create ~len:width (Value.Num Float.nan) in
      List.iteri line ~f:(fun i v -> row.(i) <- v);
      row
    )
  |> Array.of_list

let create_ctx_alist id table =
  let table_v = Value.Vec table in
  let ctx_alist =
    if id = 0 then Array.to_list table
                   |> List.mapi ~f:(fun i v -> ("$" ^ Int.to_string i, v))
    else []
  in
  let ctx_alist' =
    if id = 0 then ("$$", table_v) :: ("$$0", table_v) :: ctx_alist
    else ("$$" ^ Int.to_string id, table_v) :: ctx_alist
  in
  ctx_alist'


module Table_options = struct
  type t = {
    separator: char list [@default [' '; '\t']];
    transpose: bool [@default false];
  } [@@deriving sexp]
end


module Table = struct
  type options = Table_options.t
  let default_options = {
    Table_options.separator = [' '; '\t'];
    Table_options.transpose = false;
  }
  let options_of_sexp = Table_options.t_of_sexp

  let remove_comments lines =
    List.filter lines ~f:(fun line -> String.prefix line 1 <> "#")

  let read_from_channel opts id ch =
    let separator = opts.Table_options.separator in
    let transpose = if opts.Table_options.transpose then Fn.id else Array.transpose_exn in
    let table = In_channel.input_lines ch
                |> List.filter ~f:(fun line -> line <> "")
                |> remove_comments
                |> create_table separator
                |> transpose
                |> Array.map ~f:(fun row -> Value.Vec row)
    in
    let ctx_alist = create_ctx_alist id table in
    Eval.Context.of_alist ctx_alist
end


module Table_extended = struct
  type options = Table_options.t
  let default_options = {
    Table_options.separator = [' '; '\t'];
    Table_options.transpose = false;
  }  let options_of_sexp = Table_options.t_of_sexp

  let valid_name_re = Re2.Regex.create_exn "^[A-Za-z\\$][A-Za-z0-9\\$_']*$"
  let validate_name name = Re2.Regex.matches valid_name_re name

  let read_const consts str = match split_words [' '; '\t'] str with
    | [name; value] -> if validate_name name then
        Hashtbl.set consts ~key:name ~data:(read_num value)
    | _ -> ()

  let read_and_remove_comments consts lines =
    List.filter lines ~f:(fun line ->
        if String.prefix line 1 <> "#" then true
        else begin
          if String.prefix line 2 = "##" then read_const consts (String.drop_prefix line 2);
          false
        end
      )

  let read_from_channel opts id ch =
    let separator = opts.Table_options.separator in
    let transpose = if opts.Table_options.transpose then Fn.id else Array.transpose_exn in
    let consts = Hashtbl.create ~hashable:String.hashable () in
    let table = In_channel.input_lines ch
                |> List.filter ~f:(fun line -> line <> "")
                |> read_and_remove_comments consts
                |> create_table separator
                |> transpose
                |> Array.map ~f:(fun row -> Value.Vec row)
    in
    let ctx_alist = create_ctx_alist id table @ Hashtbl.to_alist consts in
    Eval.Context.of_alist ctx_alist
end
