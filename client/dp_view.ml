include View_intf
open Base
open Bonsai

module Coin_change_Model = struct
  type t = int * int list [@@deriving sexp, equal]
end

type t = Coin_change of Coin_change_Model.t [@@deriving typed_variants, sexp, equal]

let to_spec_name = function
  | Coin_change _ -> "coin_change"
;;

let coin_change_form =
  let open! Bonsai.Let_syntax in
  let amount = Form.Elements.Number.int ~min:1 [%here] ~default:1 ~step:1 () in
  let coin = Form.Elements.Number.int [%here] ~min:1 ~default:1 ~step:1 () in
  let coins = Form.Elements.Multiple.list [%here] coin in
  let%sub amount = amount in
  let%sub coins = coins in
  let%arr coins = coins
  and amount = amount in
  let amount = Form.label "Amount:" amount in
  let coins = Form.group "Coins:" coins in
  Form.both amount coins
;;

let form_of_v =
  Form.Typed.Variant.make
    (module struct
      module Typed_variant = Typed_variant

      let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
        = function
        | Coin_change -> coin_change_form
      ;;
    end)
;;

let handle_update _v = Effect.Ignore
