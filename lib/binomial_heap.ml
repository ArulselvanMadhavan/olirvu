include Binomial_heap_intf
open Base

module Make (C : Comparable.S) : S with module Elem := C = struct
  (* In a tree, children are in order of decreasing rank *)
  type tree = Node of int * C.t * tree list

  (* In the heap, trees are in order of increasing rank *)
  type t = tree list

  let empty = []

  let link = function
    | (Node (k, e1, xs) as t1), (Node (_, e2, ys) as t2) ->
      if C.(e1 <= e2) then Node (k + 1, e1, t2 :: xs) else Node (k + 1, e2, t1 :: ys)
  ;;

  let rank = function
    | Node (r, _, _) -> r
  ;;

  let rec ins_tree = function
    | t1, [] -> [ t1 ]
    | t1, (t2 :: xs as h) ->
      if rank t1 < rank t2 then t1 :: h else ins_tree (link (t1, t2), xs)
  ;;

  let insert (x, h) = ins_tree (Node (0, x, []), h)

  let rec merge = function
    | h, [] -> h
    | [], h -> h
    | t1 :: h1, t2 :: h2 ->
      if rank t1 < rank t2
      then t1 :: merge (h1, t2 :: h2)
      else if rank t2 < rank t1
      then t2 :: merge (t1 :: h1, h2)
      else ins_tree (link (t1, t2), merge (h1, h2))
  ;;

  let elem = function
    | Node (_, e, _) -> e
  ;;

  let children = function
    | Node (_, _, xs) -> xs
  ;;

  let rec remove_min_tree = function
    | [] -> None
    | [ t ] -> Some (t, [])
    | t1 :: ts ->
      let min_opt = remove_min_tree ts in
      let check_min = function
        | min_t, xs -> if C.(elem t1 < elem min_t) then t1, ts else min_t, t1 :: xs
      in
      if Option.is_none min_opt then Some (t1, ts) else Option.map ~f:check_min min_opt
  ;;

  let find_min h = Option.map (remove_min_tree h) ~f:(fun (min_t, _) -> elem min_t)

  let delete_min h =
    let rem_result = remove_min_tree h in
    let handle_rem _ (min_t, rem_t) =
      let xs = children min_t in
      let xs = List.rev xs in
      merge (xs, rem_t)
    in
    Option.fold rem_result ~init:[] ~f:handle_rem
  ;;

  let of_list xs = List.fold xs ~init:[] ~f:(fun acc x -> insert (x, acc))

  let edges_list h =
    (* For tree, compute edges_list. collect id and concat edges list *)
    let rec helper acc id ~parent = function
      | [] -> acc, id
      | Node (k, e, xs) :: ts ->
        let node_id = id + 1 in
        let node = parent, node_id, Some e, k in
        let acc = node :: acc in
        let acc, id = helper acc node_id ~parent:node_id xs in
        helper acc id ~parent ts
    in
    let acc, _ = helper [] 0 ~parent:(0) h in
    (* Get the layout working. Heap children in order of increasing rank *)
    acc
  ;;
end
