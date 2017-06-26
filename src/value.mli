open Core

type t = Num of float
       | Fun of (t -> t)
       | Vec of t array

val equal : t -> t -> bool
val type_string_of : t -> string
