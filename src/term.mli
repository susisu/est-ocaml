open Core

type 'a t = Lit of 'a * Value.t
          | Var of 'a * string
          | Vec of 'a * 'a t list
          | App of 'a * 'a t * 'a t
          | Let of 'a * string * 'a t * 'a t

val equal : 'a t -> 'a t -> bool
