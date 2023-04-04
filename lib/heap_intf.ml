module type Heap = sig
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

module ExplicitMin (H : Heap) : Heap = struct
  module Elem = H.Elem

  type t =
    | E
    | NE of Elem.t * H.t

  let empty = E

  let insert = function
    | e, E -> NE (e, H.insert (e, H.empty))
    | cur_min, NE (new_elem, h) ->
      let h = H.insert (new_elem, h) in
      if new_elem < cur_min then NE (new_elem, h) else NE (cur_min, h)
  ;;

  let merge = function
    | E, E -> E
    | (NE (_, _) as t1), E -> t1
    | E, (NE (_, _) as t2) -> t2
    | NE (min1, h1), NE (min2, h2) ->
      let min = if min1 < min2 then min1 else min2 in
      NE (min, H.merge (h1, h2))
  ;;

  let find_min_to_t h =
    let new_min = H.find_min h in
    Option.fold ~none:E ~some:(fun new_min -> NE (new_min, h)) new_min
  ;;

  let delete_min = function
    | E -> E
    | NE (_, h) ->
      let h = H.delete_min h in
      find_min_to_t h
  ;;

  let of_list xs =
    let h = H.of_list xs in
    if List.length xs = 0 then E else find_min_to_t h
  ;;

  let edges_list = function
    | E -> []
    | NE (_, h) -> H.edges_list h
  ;;

  let find_min = function
    | E -> None
    | NE (min_elem, _h) -> Some min_elem
  ;;
end
