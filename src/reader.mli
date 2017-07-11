open Core

module type Reader_intf = sig
  type options
  val default_options : options
  val options_of_sexp : Sexplib.Sexp.t -> options
  val read_from_channel : options -> int -> In_channel.t -> Eval.Context.t
end

exception Read_error of string


module Table_options : sig
  type t = {
    separator: char list;
    transpose: bool;
  } [@@deriving sexp]
end

module Table : Reader_intf with type options = Table_options.t

module Table_extended : Reader_intf with type options = Table_options.t
