open Core

type t = VlNum of float
       | VlFun of (t -> t)
       | VlVec of t list

type value_type = Type_number
                | Type_function
                | Type_vector

val equal : t -> t -> bool
val type_of : t -> value_type
val string_of_value_type : value_type -> string
