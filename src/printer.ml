open Core

module type Printer_intf = sig
  module Config : sig
    type options
    type t
    val options_of_sexp : Sexp.t -> options
    val empty_options : options
    val merge_options : options -> options -> options
    val of_options : default:t -> options -> t
  end

  val default_config : Config.t
  val print_to_channel : Config.t -> Out_channel.t -> Value.t -> unit
end

exception Print_error of string


module Table_config = struct
  type options = {
    strict   : bool sexp_option;
    separator: string sexp_option;
    precision: int sexp_option;
    default  : float sexp_option;
    transpose: bool sexp_option;
  } [@@deriving sexp]

  type t = {
    strict   : bool;
    separator: string;
    precision: int;
    default  : float;
    transpose: bool;
  }

  let empty_options : options = {
    strict    = None;
    separator = None;
    precision = None;
    default   = None;
    transpose = None;
  }

  let merge_options (opt1 : options) (opt2 : options) : options = {
    strict    = Option.first_some opt2.strict opt1.strict;
    separator = Option.first_some opt2.separator opt1.separator;
    precision = Option.first_some opt2.precision opt1.precision;
    default   = Option.first_some opt2.default opt1.default;
    transpose = Option.first_some opt2.transpose opt1.transpose;
  }

  let of_options ~default (opt : options) = {
    strict    = Option.value ~default:default.strict opt.strict;
    separator = Option.value ~default:default.separator opt.separator;
    precision = Option.value ~default:default.precision opt.precision;
    default   = Option.value ~default:default.default opt.default;
    transpose = Option.value ~default:default.transpose opt.transpose;
  }
end

module Table = struct
  module Config = Table_config

  let default_config = Config.({
      strict    = true;
      separator = "\t";
      precision = 8;
      default   = Float.nan;
      transpose = false;
    })

  let rec rank = function
    | Value.Num _ | Value.Fun _ -> 0
    | Value.Vec vec ->
      if Array.length vec = 0 then 1
      else Array.fold vec ~init:0 ~f:(fun r elem ->
          let r' = rank elem + 1 in
          if r = 0 || r = r' then r'
          else raise (Print_error "not structured data")
        )

  let raise_unexpected name = raise (Print_error ("unexpected " ^ name))

  let print_number prec num = sprintf "%.*g" prec num
  let print_endline_to_channel ch line = Out_channel.output_string ch (line ^ "\n")

  let rank0_to_string prec = function
    | Value.Num num -> print_number prec num
    | Value.Fun _ -> raise_unexpected "function"
    | Value.Vec _ -> raise_unexpected "vector"

  let print_rank0 prec ch v = print_endline_to_channel ch (rank0_to_string prec v)

  let print_rank1 sep prec trans ch = function
    | Value.Num _ -> raise_unexpected "number"
    | Value.Fun _ -> raise_unexpected "function"
    | Value.Vec vec ->
      let col = Array.map vec ~f:(rank0_to_string prec) in
      if trans then String.concat_array ~sep col |> print_endline_to_channel ch
      else Array.iter col ~f:(print_endline_to_channel ch)

  let print_rank2 strict sep prec default trans ch = function
    | Value.Num _ -> raise_unexpected "number"
    | Value.Fun _ -> raise_unexpected "function"
    | Value.Vec vec ->
      let table = Array.map vec ~f:(function
          | Value.Num _ -> raise_unexpected "number"
          | Value.Fun _ -> raise_unexpected "function"
          | Value.Vec vec -> Array.map vec ~f:(rank0_to_string prec)
        )
      in
      let width = Array.length table in
      let heights = Array.map table ~f:Array.length in
      let height = Array.fold heights ~init:0 ~f:Int.max in
      if strict && Array.exists heights ~f:(fun h -> h <> height) then
        raise (Print_error "table is incomplete");
      let default_str = print_number prec default in
      if trans then
        Array.iter table ~f:(fun row ->
            let line_arr = Array.create ~len:height default_str in
            Array.iteri row ~f:(fun i elem -> line_arr.(i) <- elem);
            String.concat_array ~sep line_arr |> print_endline_to_channel ch
          )
      else
        for i = 0 to height - 1 do
          let line_arr = Array.create ~len:width default_str in
          Array.iteri table ~f:(fun j col -> if i < heights.(j) then line_arr.(j) <- col.(i));
          String.concat_array ~sep line_arr |> print_endline_to_channel ch
        done

  let print_to_channel config ch v =
    let {
      Config.strict;
      Config.separator = sep;
      Config.precision = prec;
      Config.default;
      Config.transpose = trans;
    } = config in
    match rank v with
    | 0 -> print_rank0 prec ch v
    | 1 -> print_rank1 sep prec trans ch v
    | 2 -> print_rank2 strict sep prec default trans ch v
    | _ -> raise (Print_error (
        "structure is too complex\n" ^
        "expecting scalar, vector or matrix"
      ))
end
