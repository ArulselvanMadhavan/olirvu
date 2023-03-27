open Base
include Heap_intf

module Make (C : Comparable.S) : S with module Elem := C = struct
  type t =
    | E
    | T of int * C.t * t * t

  let empty = E

  let rank = function
    | E -> 0
    | T (r, _, _, _) -> r
  ;;

  let makeT x l r =
    if rank l >= rank r then T (rank r + 1, x, l, r) else T (rank l + 1, x, r, l)
  ;;

  let rec merge = function
    | h, E -> h
    | E, h -> h
    | (T (_, x, a1, b1) as h1), (T (_, y, a2, b2) as h2) ->
      if C.(x <= y) then makeT x a1 (merge (b1, h2)) else makeT y a2 (merge (h1, b2))
  ;;

  let insert (x, h) = merge (T (1, x, E, E), h)

  let find_min = function
    | E -> None
    | T (_, x, _, _) -> Some x
  ;;

  let delete_min = function
    | E -> E
    | T (_, _, a, b) -> merge (a, b)
  ;;
end
