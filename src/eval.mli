open Core

exception Runtime_error of string

module Context : sig
  type t

  val empty : t
  val of_alist : (string * Value.t) list -> t
end

module Make_eval(Info : Stringable.S) : sig
  val eval : Context.t -> Info.t Term.t -> Value.t
end
