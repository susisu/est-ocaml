open Core

type 'a t = Lit of 'a * Value.t
          | Var of 'a * string
          | Vec of 'a * 'a t list
          | App of 'a * 'a t * 'a t
          | Let of 'a * string * 'a t * 'a t


let rec equal t1 t2 = match (t1, t2) with
  | (Lit (_, v1), Lit (_, v2)) -> Value.equal v1 v2
  | (Lit _, _) -> false
  | (Var (_, name1), Var (_, name2)) -> String.equal name1 name2
  | (Var _, _) -> false
  | (Vec (_, elems1), Vec (_, elems2)) ->
    begin
      let module U = List.Or_unequal_lengths in
      match List.for_all2 elems1 elems2 ~f:equal with
      | U.Ok r -> r
      | U.Unequal_lengths -> false
    end
  | (Vec _, _) -> false
  | (App (_, func1, arg1), App (_, func2, arg2)) -> equal func1 func2 && equal arg1 arg2
  | (App _, _) -> false
  | (Let (_, name1, expr1, body1), Let (_, name2, expr2, body2)) ->
    String.equal name1 name2 && equal expr1 expr2 && equal body1 body2
  | (Let _, _) -> false

let get_info = function
  | Lit (info, _) -> info
  | Var (info, _) -> info
  | Vec (info, _) -> info
  | App (info, _, _) -> info
  | Let (info, _, _, _) -> info
