open Core

module type Reader = sig
  type options
  val default_options : options
  val options_of_sexp : Sexplib.Sexp.t -> options
  val read_from_channel : options -> int -> In_channel.t -> Eval.Context.t
end


module Table_options : sig
  type t = {
    transpose: bool;
  } [@@deriving sexp]
end

module Table : sig
  type options = Table_options.t
  include Reader with type options := Table_options.t
end

module Table_extended : sig
  type options = Table_options.t
  include Reader with type options := Table_options.t
end
