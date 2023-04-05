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
    | Heaps of Heap_view.t
    | DynamicProgramming of Dp_view.t
  [@@deriving typed_variants]
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
        | Heaps -> Heap_view.form_of_v
        | DynamicProgramming -> Dp_view.form_of_v
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
  match v with
  | Ok (V.Heaps h) ->
    let spec_name = Heap_view.to_spec_name h in
    fetch_spec inject spec_name
  | Ok (V.DynamicProgramming d) ->
    let spec_name = Dp_view.to_spec_name d in
    fetch_spec inject spec_name
  | Error e -> inject (A.Error (Some e))
;;

let handle_update_viz inject v _e =
  match v with
  | Ok (V.Heaps h) -> Heap_view.handle_update h
  | Ok (V.DynamicProgramming d) -> Dp_view.handle_update d
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
