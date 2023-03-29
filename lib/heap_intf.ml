module type S = sig
  module Elem : Base.Comparable.S

  type t

  val empty : t
  val insert : Elem.t * t -> t
  val merge : t * t -> t
  val find_min : t -> Elem.t Option.t
  val delete_min : t -> t
  val of_list : Elem.t list -> t
  val edges_list : t -> (int * int * Elem.t Option.t * int) list
end
