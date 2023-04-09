open! Base
open! Bonsai_web
module Form = Bonsai_web_ui_form

type t = float * float [@@deriving sexp, equal]

let to_spec_name _ = "quant_diff_full.vg.json"

let form_of_v =
  let open Bonsai.Let_syntax in
  let value = Form.Elements.Number.float [%here] ~default:0. ~step:1. () in
  let max_val = Form.Elements.Number.float [%here] ~default:1. ~step:1. () in
  let%sub value = value in
  let%sub max_val = max_val in
  let%arr value = value
  and max_val = max_val in
  Form.both value max_val
;;

let handle_update (fp32_val, max_val) =
  Stdio.printf "%f|%f\n" fp32_val max_val;
  Effect.Ignore
;;
