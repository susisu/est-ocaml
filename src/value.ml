open Core

type t = Number of float
       | Function of (t -> t)
       | Vector of t list

type value_type = Type_number
                | Type_function
                | Type_vector


let rec equal v1 v2 = match (v1, v2) with
  | (Number num1, Number num2) -> Float.equal num1 num2
  | (Function fun1, Function fun2) -> phys_equal fun1 fun2
  | (Vector vec1, Vector vec2) ->
    begin
      let module M = Core.List.Or_unequal_lengths in
      match List.for_all2 vec1 vec2 ~f:equal with
      | M.Ok r -> r
      | M.Unequal_lengths -> false
    end
  | _ -> false

let type_of = function
  | Number _ -> Type_number
  | Function _ -> Type_function
  | Vector _ -> Type_vector

let string_of_value_type = function
  | Type_number -> "number"
  | Type_function -> "function"
  | Type_vector -> "vector"
