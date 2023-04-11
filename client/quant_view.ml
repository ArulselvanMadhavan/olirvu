open! Base
open! Bonsai_web
module Form = Bonsai_web_ui_form

type t = float list [@@deriving sexp, equal]

let to_spec_name _ = "quant_diff_full"

let form_of_v =
  let value = Form.Elements.Number.float [%here] ~default:0. ~step:0.1 () in
  let values = Form.Elements.Multiple.list [%here] value in
  values
;;

let handle_update xs =
  if List.is_empty xs then Effect.Ignore else(
  let open Olirvu in
  let module E5M2 = Quantization.FP32_to_FP_Q (Quant_intf.E5M2) in
  let module E4M3 = Quantization.FP32_to_FP_Q (Quant_intf.E4M3) in
  let module E3M4 = Quantization.FP32_to_FP_Q (Quant_intf.E3M4) in
  let module INT8 = Quantization.FP32_to_INT_Q (Quant_intf.INT8) in
  let module VSQ = Quantization.FP32_to_VSQ (Quant_intf.VSQ_V16) in
  let fp_result =
    [ "FP32", xs
    ; "E5M2", E5M2.quantize ~fp32:xs
    ; "E4M3", E4M3.quantize ~fp32:xs
    ; "E3M4", E3M4.quantize ~fp32:xs
    ]
  in
  let int_result = [ "INT8", INT8.quantize ~fp32:xs ] in
  let int_float_result = ["VSQ", VSQ.quantize ~fp32:xs ] in
  Effect.return (Vega.build_quantized_view fp_result int_result int_float_result))
;;
