open Core

module type Base = sig
  type options

  val read : options -> string -> Eval.Context.t
end

module Make_reader(B : Base) : sig
  type options = B.options

  val read : options -> string -> Eval.Context.t
  val read_from_channel : options -> In_channel.t -> Eval.Context.t
end


type table_options = {
  id: int;
  transpose: bool;
}

module Table : sig
  type options = table_options
  include Base with type options := table_options
end

module Table_extended : sig
  type options = table_options
  include Base with type options := table_options
end
