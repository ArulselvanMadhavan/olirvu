open! Base
open! Bonsai
open! Bonsai_web
open! Async_kernel
open! Async_js
open! Js_of_ocaml
open! Core
include View_intf

type t =
  | Red_Black_Tree of Forms.Heap_Model.t
  | Leftist_Heap of Forms.Heap_Model.t
  | Binomial_Heap of Forms.Heap_Model.t
[@@deriving typed_variants, sexp, equal]

let to_spec_name = function
  | Red_Black_Tree _ -> "red_black_tree"
  | Leftist_Heap _ -> "leftist_heap"
  | Binomial_Heap _ -> "binomial_heap"
;;

let form_of_v =
  Form.Typed.Variant.make
    (module struct
      module Typed_variant = Typed_variant

      let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
        = function
        | Red_Black_Tree -> Forms.heap_form
        | Leftist_Heap -> Forms.heap_form
        | Binomial_Heap -> Forms.heap_form
      ;;
    end)
;;

let handle_update v =
  let open Olirvu in
  match v with
  | Red_Black_Tree xs ->
    let tree = List.fold xs ~init:Rbt.E ~f:(Fn.flip Rbt.insert) in
    Effect.return (Vega.build_rbt tree)
  | Leftist_Heap xs -> Effect.return (Vega.build_heap xs)
  | Binomial_Heap xs -> Effect.return (Vega.build_bin_heap xs)
;;
