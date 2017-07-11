open Core

module type Showable = sig
  type t
  val to_string : t -> string
end

module Position = struct
  open Lexing

  type t = position

  let to_string p =
    let fname = p.pos_fname in
    let lnum = p.pos_lnum in
    let cnum = p.pos_cnum - p.pos_bol + 1 in
    let fname_prefix =
      if fname = "" then ""
      else "\"" ^ fname ^ "\""
    in
    sprintf "%s(line %d, column %d)" fname_prefix lnum cnum
end

let indent size str = String.split_lines str
                      |> List.map ~f:(fun line -> String.make size ' ' ^ line)
                      |> String.concat ~sep:"\n"

let format_error_message msg detail =
  if detail = "" then msg
  else msg ^ ":\n" ^ indent 2 detail
