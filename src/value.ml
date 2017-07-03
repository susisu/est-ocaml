open Core

type t = Num of float
       | Fun of (t -> t)
       | Vec of t array


let rec equal v1 v2 = match (v1, v2) with
  | (Num num1, Num num2) -> num1 = num2
  | (Num _, _) -> false
  | (Fun fun1, Fun fun2) -> phys_equal fun1 fun2
  | (Fun _, _) -> false
  | (Vec vec1, Vec vec2) ->
    begin
      try Array.for_all2_exn vec1 vec2 ~f:equal with
      | Invalid_argument _ -> false
    end
  | (Vec _, _) -> false

let type_string_of = function
  | Num _ -> "number"
  | Fun _ -> "function"
  | Vec _ -> "vector"

let rec to_string = function
  | Num num -> Float.to_string num
  | Fun _ -> "<function>"
  | Vec vec ->
    let elems_str = String.concat_array ~sep:", " (Array.map vec ~f:to_string) in
    "[" ^ elems_str ^ "]"
