open Core

exception Runtime_error of string option * string

module Context : sig
  type t
  val empty : t
  val of_alist : (string * Value.t) list -> t
  val append : t -> t -> t
end

module Make_eval(Info : Common.Showable) : sig
  val eval : Context.t -> Info.t Term.t -> Value.t
end
