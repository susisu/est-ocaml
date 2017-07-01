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
      if String.is_empty fname then ""
      else "\"" ^ fname ^ "\""
    in
    sprintf "%s(line %d, column %d)" fname_prefix lnum cnum
end
