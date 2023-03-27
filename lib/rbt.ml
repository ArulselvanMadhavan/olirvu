type color =
  | R
  | B

let to_string = function
  | R -> "red"
  | B -> "black"
;;

type 'a t =
  | E
  | T of color * 'a t * 'a * 'a t

(* let rec member x = function *)
(*   | E -> false *)
(*   | T (_, l, e, r) -> if x < e then member x l else if x > e then member x r else true *)
(* ;; *)

let balance = function
  | B, T (R, T (R, a, x, b), y, c), z, d
  | B, T (R, a, x, T (R, b, y, c)), z, d
  | B, a, x, T (R, b, y, T (R, c, z, d))
  | B, a, x, T (R, T (R, b, y, c), z, d) -> T (R, T (B, a, x, b), y, T (B, c, z, d))
  | c, l, e, r -> T (c, l, e, r)
;;

let insert x t =
  let rec ins = function
    | E -> T (R, E, x, E)
    | T (c, l, e, r) as s ->
      if x < e
      then balance (c, ins l, e, r)
      else if x > e
      then balance (c, l, e, ins r)
      else s
  in
  match ins t with
  | T (_, l, e, r) -> T (B, l, e, r)
  | _ -> t (* Never happens *)
;;

let rec count = function
  | E -> 0
  | T (_, l, _, r) -> 1 + count l + count r
;;
