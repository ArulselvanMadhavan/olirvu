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

  (* let of_list xs = *)
  (*   List.fold xs ~init:E ~f:(fun acc x -> insert (x, acc)) *)

  let of_list xs =
    (* Ex 3.3 *)
    let xs = List.map xs ~f:(fun x -> insert (x, E)) in
    let xs = ref xs in
    let rec merge_adj = function
      | [] -> []
      | x :: [] -> [ x ]
      | x1 :: x2 :: xs -> merge (x1, x2) :: merge_adj xs
    in
    while List.length !xs > 1 do
      xs := merge_adj !xs
    done;
    let xs = !xs in
    List.hd_exn xs

  let edges_list t =
    let rec helper acc id ~parent = function
      | E -> acc, id
      | T (rk, x, l, r) ->
        let node_id = id + 1 in
        let acc, id = helper ((parent, node_id, x, rk) :: acc) node_id ~parent:node_id l in
        helper acc id ~parent:node_id r
    in
    let acc, _ = helper [] 0 ~parent:0 t in
    List.rev acc
  ;;
end
