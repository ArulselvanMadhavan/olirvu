open! Base
open! Bonsai
open! Bonsai_web
open! Async_kernel
open! Async_js
open! Js_of_ocaml
module Form = Bonsai_web_ui_form

module M = struct
  (* Model *)
  type t =
    { spec : string option
    ; error : Error.t option
    }
  [@@deriving sexp, equal]

  let default = { spec = None; error = None }
end

module RBT_Model = struct
  type t = int list [@@deriving sexp, equal]
end

module RBT_Action = struct
  type t = Add [@@deriving sexp]
end

module V = struct
  type t = Red_Black_Tree of RBT_Model.t [@@deriving typed_variants, sexp, equal]

  let to_spec_name = function
    | Red_Black_Tree _ -> "red_black_tree"
  ;;
end

module A = struct
  type t =
    | Spec of string option
    | Error of Error.t option
  [@@deriving sexp_of]
end

let rbtree_form =
  let open! Bonsai.Let_syntax in
  let int_form = Form.Elements.Number.int [%here] ~default:0 ~step:1 () in
  Form.Elements.Multiple.list [%here] int_form
;;

(* let f = Value.return (fun xs -> Effect.print_s @@ RBT_Model.sexp_of_t xs) in *)
(* let%sub () = Form.Dynamic.on_change (module RBT_Model) list_form ~f in *)

let form_of_v =
  Form.Typed.Variant.make
    (module struct
      module Typed_variant = V.Typed_variant

      let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
        = function
        | Red_Black_Tree -> rbtree_form
      ;;
    end)
;;

let handle_spec_change s =
  let json_spec =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "JSON.parse")
      [| Js.Unsafe.inject (Js.string s) |]
  in
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

let handle_v_change inject v =
  let spec_name = V.to_spec_name v in
  fetch_spec inject spec_name
;;

let on_submit =
  Form.Submit.create
    ~button:(Some "Visualize")
    ~f:(fun (V.Red_Black_Tree xs) -> Effect.return @@ Vega.view_insert xs)
    ()
;;

let view_of_form : Vdom.Node.t Computation.t =
  let open! Bonsai.Let_syntax in
  let%sub _state, inject =
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
  let%sub () =
    Form.Dynamic.on_change
      (module V)
      form_v
      ~f:(Value.map inject ~f:(fun inject -> handle_v_change inject))
  in
  let%arr form_v = form_v in
  (* and state = state in *)
  Vdom.Node.div [ Form.view_as_vdom form_v ~on_submit ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" view_of_form
;;
