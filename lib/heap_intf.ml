module type S = sig
  module Elem : Base.Comparable.S

  type t

  val empty : t
  val insert : Elem.t * t -> t
  val merge : t * t -> t
  val find_min : t -> Elem.t Option.t
  val delete_min : t -> t
end

module type Heap = sig
  module Make (T : Base.Comparable.S) : S with module Elem := T
end
