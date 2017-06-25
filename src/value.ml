open Core

type t = Num of float
       | Fun of (t -> t)
       | Vec of t list


let rec equal v1 v2 = match (v1, v2) with
  | (Num num1, Num num2) -> Float.equal num1 num2
  | (Num _, _) -> false
  | (Fun fun1, Fun fun2) -> phys_equal fun1 fun2
  | (Fun _, _) -> false
  | (Vec vec1, Vec vec2) ->
    begin
      let module U = Core.List.Or_unequal_lengths in
      match List.for_all2 vec1 vec2 ~f:equal with
      | U.Ok r -> r
      | U.Unequal_lengths -> false
    end
  | (Vec _, _) -> false

let type_string_of = function
  | Num _ -> "number"
  | Fun _ -> "function"
  | Vec _ -> "vector"
