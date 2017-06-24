open Core

type t = VlNum of float
       | VlFun of (t -> t)
       | VlVec of t list

type value_type = Type_number
                | Type_function
                | Type_vector


let rec equal v1 v2 = match (v1, v2) with
  | (VlNum num1, VlNum num2) -> Float.equal num1 num2
  | (VlNum _, _) -> false
  | (VlFun fun1, VlFun fun2) -> phys_equal fun1 fun2
  | (VlFun _, _) -> false
  | (VlVec vec1, VlVec vec2) ->
    begin
      let module U = Core.List.Or_unequal_lengths in
      match List.for_all2 vec1 vec2 ~f:equal with
      | U.Ok r -> r
      | U.Unequal_lengths -> false
    end
  | (VlVec _, _) -> false

let type_of = function
  | VlNum _ -> Type_number
  | VlFun _ -> Type_function
  | VlVec _ -> Type_vector

let string_of_value_type = function
  | Type_number -> "number"
  | Type_function -> "function"
  | Type_vector -> "vector"
