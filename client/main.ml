open! Base
open! Bonsai
open! Bonsai_web
open! Async_kernel
open! Async_js
open! Js_of_ocaml
open! Core
module Form = Bonsai_web_ui_form

module M = struct
  (* Model *)
  type t =
    { spec : string option
    ; error : Error.t option
    }
  [@@deriving sexp, equal, fields]

  let default = { spec = None; error = None }
end

module V = struct
  type t =
    | Red_Black_Tree of Forms.RBT_Model.t
    | Leftist_Heap of Forms.RBT_Model.t
  [@@deriving typed_variants, sexp, equal]

  let to_spec_name = function
    | Red_Black_Tree _ -> "red_black_tree"
    | Leftist_Heap _ -> "leftist_heap"
  ;;
end

module A = struct
  type t =
    | Spec of string option
    | Error of Error.t option
  [@@deriving sexp_of]
end

let form_of_v =
  Form.Typed.Variant.make
    (module struct
      module Typed_variant = V.Typed_variant

      let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
        = function
        | Red_Black_Tree -> Forms.rbtree_form
        | Leftist_Heap -> Forms.rbtree_form
      ;;
    end)
;;

let handle_spec_change s =
  let json_spec = Vega.json_parse s in
  let _ = Vega.vega_embed json_spec in
  ()
;;

let fetch_spec inject spec_name =
  let open Effect.Let_syntax in
  let%bind response =
    Effect.of_deferred_fun
      (fun p -> Async_js.Http.get ~arguments:[] p)
      ("/recipe/" ^ spec_name ^ ".vg.json")
  in
  if Core.Or_error.is_error response
  then inject (A.Error (Core.Result.error response))
  else (
    let spec = Core.Or_error.ok_exn response in
    handle_spec_change spec;
    inject (A.Spec (Some spec)))
;;

let on_viz_click inject v _ =
  (* if viz_visible *)
  (* then Effect.Ignore *)
  (* else ( *)
  match v with
  | Ok v ->
    let spec_name = V.to_spec_name v in
    fetch_spec inject spec_name
  | Error e -> inject (A.Error (Some e))
;;

let handle_update_viz inject v _e =
  let open Olirvu in
  match v with
  | Ok (V.Red_Black_Tree xs) ->
    let tree = List.fold xs ~init:Rbt.E ~f:(Fn.flip Rbt.insert) in
    Effect.return (Vega.build_rbt tree)
  | Ok (V.Leftist_Heap xs) -> Effect.return (Vega.build_heap xs)
  | Error e -> inject (A.Error (Some e))
;;

let view_of_form : Vdom.Node.t Computation.t =
  let open! Bonsai.Let_syntax in
  let open Vdom in
  let%sub state, inject =
    Bonsai.state_machine0
      [%here]
      (module M)
      (module A)
      ~default_model:M.default
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model action ->
        match action with
        | Spec s -> { model with spec = s }
        | Error e -> { model with error = e })
  in
  let%sub form_v = form_of_v in
  let%arr form_v = form_v
  and inject = inject
  and state = state in
  let v = Form.value form_v in
  let viz_visible = M.spec state |> Option.is_some in
  let viz_btn_text = if viz_visible then "Refresh Viz" else "Show Viz" in
  let update_viz =
    if viz_visible
    then
      Node.button
        ~attr:(Attr.on_click (handle_update_viz inject v))
        [ Node.Text "Update Viz" ]
    else Node.None
  in
  let viz_btn =
    Node.button ~attr:(Attr.on_click (on_viz_click inject v)) [ Node.Text viz_btn_text ]
  in
  Node.div [ Form.view_as_vdom form_v; update_viz; viz_btn ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" view_of_form
;;
