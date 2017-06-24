open Core

type 'a t = TmLit of 'a * Value.t
          | TmVar of 'a * string
          | TmVec of 'a * 'a t list
          | TmApp of 'a * 'a t * 'a t
          | TmLet of 'a * string * 'a t * 'a t

val equal : 'a t -> 'a t -> bool
