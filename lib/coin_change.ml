open Base

let handle_prev c_coins p_coins = Option.fold c_coins ~init:(p_coins + 1) ~f:Int.min

(* Array.make [0,amount] *)
(* for every amount, consider all coins *)
(* if the amount is greater than coin, min(num_coins(current_amount - coin) + 1, num_coins[current_amount]  *)
let coin_change amount coins =
  let coins = Array.of_list coins in
  let dp = Array.create ~len:(amount + 1) None in
  dp.(0) <- Some 0;
  let coins_end = Array.length coins - 1 in
  for i = 0 to amount do
    for j = 0 to coins_end do
      let coin = coins.(j) in
      let str = Printf.sprintf "%d|%d\n" i coin in
      Stdlib.print_string str;
      if i >= coin
      then (
        let p_coins = dp.(i - coin) in
        let c_coins = dp.(i) in
        dp.(i) <- Option.map p_coins ~f:(handle_prev c_coins))
    done
  done;
  dp
;;
