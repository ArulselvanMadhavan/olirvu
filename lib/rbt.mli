type color =
  | R
  | B

val to_string : color -> string

type 'a t =
  | E
  | T of color * 'a t * 'a * 'a t

val insert : 'a -> 'a t -> 'a t
val count : 'a t -> int
