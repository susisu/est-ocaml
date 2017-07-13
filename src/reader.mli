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

  val read_from_channel : Config.t -> int -> In_channel.t -> Eval.Context.t
end

exception Read_error of string


module Table_config : sig
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

  val empty_options : options
  val merge_options : options -> options -> options
  val of_options : default:t -> options -> t
end

module Table : Reader_intf with module Config = Table_config
module Table_ex : Reader_intf with module Config = Table_config
