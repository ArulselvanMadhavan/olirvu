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

let handle_update (fp32, max_val) =
  let open Olirvu in
  let module E5M2 = Quantization.FP32_to_FP8 (Quant_intf.E5M2) in
  Stdio.printf "%f|%f|%f\n" fp32 max_val (E5M2.quantize ~fp32);
  Effect.Ignore
;;
