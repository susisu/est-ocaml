open Core

module type Reader_intf = sig
  module Config : sig
    type options
    type t
    val options_of_sexp : Sexp.t -> options
    val empty_options : options
    val merge_options : options -> options -> options
    val of_options : default:t -> options -> t
  end

  val default_config : Config.t
  val read_from_channel : Config.t -> int -> In_channel.t -> Eval.Context.t
end

exception Read_error of string


module Table_config = struct
  type options = {
    strict   : bool sexp_option;
    separator: char list sexp_option;
    default  : float sexp_option;
    transpose: bool sexp_option;
  } [@@deriving sexp]

  type t = {
    strict   : bool;
    separator: char list;
    default  : float;
    transpose: bool;
  }

  let empty_options : options = {
    strict    = None;
    separator = None;
    default   = None;
    transpose = None;
  }

  let merge_options (opt1 : options) (opt2 : options) : options = {
    strict    = Option.first_some opt2.strict opt1.strict;
    separator = Option.first_some opt2.separator opt1.separator;
    default   = Option.first_some opt2.default opt1.default;
    transpose = Option.first_some opt2.transpose opt1.transpose;
  }

  let of_options ~default (opt : options) = {
    strict    = Option.value ~default:default.strict opt.strict;
    separator = Option.value ~default:default.separator opt.separator;
    default   = Option.value ~default:default.default opt.default;
    transpose = Option.value ~default:default.transpose opt.transpose;
  }
end


let split_words sep str = String.split_on_chars ~on:sep str
                          |> List.filter ~f:(fun word -> word <> "")

let read_num strict default str =
  let num = try Float.of_string str with
    | Invalid_argument _ ->
      if strict then raise (Read_error ("cannot read as a number: " ^ str))
      else default
  in
  Value.Num num

let read_line strict sep default line =
  split_words sep line |> List.map ~f:(read_num strict default)

let create_table strict sep default lines =
  let lines = List.map lines ~f:(read_line strict sep default) in
  let widths = List.map lines ~f:List.length in
  let width = List.fold widths ~init:0 ~f:Int.max in
  if strict && List.exists widths ~f:(fun w -> w <> width) then
    raise (Read_error "table is incomplete");
  List.map lines ~f:(fun line ->
      let row = Array.create ~len:width (Value.Num default) in
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


module Table = struct
  module Config = Table_config

  let default_config = Config.({
      strict    = true;
      separator = [' '; '\t'];
      default   = Float.nan;
      transpose = false;
    })

  let remove_comments lines =
    List.filter lines ~f:(fun line -> String.prefix line 1 <> "#")

  let read_from_channel config id ch =
    let {
      Config.strict;
      Config.separator = sep;
      Config.default;
      Config.transpose = trans;
    } = config in
    let transpose = if trans then Fn.id else Array.transpose_exn in
    let table = In_channel.input_lines ch
                |> List.filter ~f:(fun line -> line <> "")
                |> remove_comments
                |> create_table strict sep default
                |> transpose
                |> Array.map ~f:(fun row -> Value.Vec row)
    in
    let ctx_alist = create_ctx_alist id table in
    Eval.Context.of_alist ctx_alist
end


module Table_ex = struct
  module Config = Table_config

  let default_config = Config.({
      strict    = true;
      separator = [' '; '\t'];
      default   = Float.nan;
      transpose = false;
    })

  let valid_name_re = Re2.Regex.create_exn "^[A-Za-z\\$][A-Za-z0-9\\$_']*$"
  let valid_name name = Re2.Regex.matches valid_name_re name

  let read_const consts strict default str =
    match split_words [' '; '\t'] str with
    | [name; value] ->
      if valid_name name then
        Hashtbl.set consts ~key:name ~data:(read_num strict default value)
      else if strict then
        raise (Read_error ("invalid name: " ^ name))
    | _ -> ()

  let read_and_remove_comments consts strict default lines =
    List.filter lines ~f:(fun line ->
        if String.prefix line 1 <> "#" then true
        else begin
          if String.prefix line 2 = "##" then
            read_const consts strict default (String.drop_prefix line 2);
          false
        end
      )

  let read_from_channel config id ch =
    let {
      Config.strict;
      Config.separator = sep;
      Config.default;
      Config.transpose = trans;
    } = config in
    let transpose = if trans then Fn.id else Array.transpose_exn in
    let consts = Hashtbl.create ~hashable:String.hashable () in
    let table = In_channel.input_lines ch
                |> List.filter ~f:(fun line -> line <> "")
                |> read_and_remove_comments consts strict default
                |> create_table strict sep default
                |> transpose
                |> Array.map ~f:(fun row -> Value.Vec row)
    in
    let ctx_alist = create_ctx_alist id table @ Hashtbl.to_alist consts in
    Eval.Context.of_alist ctx_alist
end
