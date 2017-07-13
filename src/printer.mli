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


module Table_config : sig
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

  val empty_options : options
  val merge_options : options -> options -> options
  val of_options : default:t -> options -> t
end

module Table : Printer_intf with module Config = Table_config
