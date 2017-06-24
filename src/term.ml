open Core

type 'a t = TmLit of 'a * Value.t
          | TmVar of 'a * string
          | TmVec of 'a * 'a t list
          | TmApp of 'a * 'a t * 'a t
          | TmLet of 'a * string * 'a t * 'a t


let rec equal t1 t2 = match (t1, t2) with
  | (TmLit (_, v1), TmLit (_, v2)) -> Value.equal v1 v2
  | (TmLit _, _) -> false
  | (TmVar (_, name1), TmVar (_, name2)) -> String.equal name1 name2
  | (TmVar _, _) -> false
  | (TmVec (_, elems1), TmVec (_, elems2)) ->
    begin
      let module U = List.Or_unequal_lengths in
      match List.for_all2 elems1 elems2 ~f:equal with
      | U.Ok r -> r
      | U.Unequal_lengths -> false
    end
  | (TmVec _, _) -> false
  | (TmApp (_, func1, arg1), TmApp (_, func2, arg2)) ->
    equal func1 func2 && equal arg1 arg2
  | (TmApp _, _) -> false
  | (TmLet (_, name1, expr1, body1), TmLet (_, name2, expr2, body2)) ->
    String.equal name1 name2 && equal expr1 expr2 && equal body1 body2
  | (TmLet _, _) -> false
