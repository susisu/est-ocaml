open Core

module type Printer_intf = sig
  type options
  val default_options : options
  val options_of_sexp : Sexplib.Sexp.t -> options
  val print_to_channel : options -> Out_channel.t -> Value.t -> unit
end

exception Ill_formed_output of string


module Table_options : sig
  type t = {
    separator: string;
    precision: int;
    transpose: bool;
  } [@@deriving sexp]
end

module Table : Printer_intf with type options = Table_options.t
