open Core

module type Printer_intf = sig
  type options
  val default_options : options
  val options_of_sexp : Sexplib.Sexp.t -> options
  val print_to_channel : options -> Out_channel.t -> Value.t -> unit
end

exception Print_error of string


module Table_options : sig
  type t = {
    strict   : bool;
    separator: string;
    precision: int;
    default  : float;
    transpose: bool;
  } [@@deriving sexp]
end

module Table : Printer_intf with type options = Table_options.t
