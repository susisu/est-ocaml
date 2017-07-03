module type Showable = sig
  type t

  val to_string : t -> string
end

module Position : sig
  type t = Lexing.position
  include Showable with type t := Lexing.position
end
