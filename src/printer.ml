open Core

module type Printer = sig
  type options
  val default_options : options
  val options_of_sexp : Sexplib.Sexp.t -> options
  val print_to_channel : options -> Out_channel.t -> Value.t -> unit
end

exception Ill_formed_output of string


module Table_options = struct
  type t = {
    separator: string [@default "\t"];
    precision: int [@default 8];
    transpose: bool [@default false];
  } [@@deriving sexp]
end

module Table = struct
  type options = Table_options.t
  let default_options = {
    Table_options.separator = "\t";
    Table_options.precision = 8;
    Table_options.transpose = false;
  }
  let options_of_sexp = Table_options.t_of_sexp

  let rec rank = function
    | Value.Num _ | Value.Fun _ -> 0
    | Value.Vec vec ->
      if Array.length vec = 0 then 1
      else Array.fold vec ~init:0 ~f:(fun r elem ->
          let r' = rank elem + 1 in
          if r = 0 || r = r' then r'
          else raise (Ill_formed_output "not structured data")
        )

  let raise_unexpected name = raise (Ill_formed_output ("unexpected " ^ name))

  let print_endline_to_channel ch line = Out_channel.output_string ch (line ^ "\n")

  let rank0_to_string prec = function
    | Value.Num num -> sprintf "%.*f" prec num
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

  let print_rank2 sep prec trans ch = function
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
      let col_heights = Array.map table ~f:Array.length in
      let height = Array.fold col_heights ~init:0 ~f:Int.max in
      if trans then
        Array.iter table ~f:(fun row ->
            let line_arr = Array.create ~len:height "nan" in
            Array.iteri row ~f:(fun i elem -> line_arr.(i) <- elem);
            String.concat_array ~sep line_arr |> print_endline_to_channel ch
          )
      else
        for i = 0 to height - 1 do
          let line_arr = Array.create ~len:width "nan" in
          Array.iteri table ~f:(fun j col -> if i < col_heights.(j) then line_arr.(j) <- col.(i));
          String.concat_array ~sep line_arr |> print_endline_to_channel ch
        done

  let print_to_channel opts ch v =
    let sep = opts.Table_options.separator in
    let prec = opts.Table_options.precision in
    let trans = opts.Table_options.transpose in
    match rank v with
    | 0 -> print_rank0 prec ch v
    | 1 -> print_rank1 sep prec trans ch v
    | 2 -> print_rank2 sep prec trans ch v
    | r -> raise (Ill_formed_output ("rank is too high: " ^ Int.to_string r))
end
