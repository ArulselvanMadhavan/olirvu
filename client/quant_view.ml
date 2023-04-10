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

(* let max_val = Form.Elements.Number.float [%here] ~default:1. ~step:1. () in *)
(* let%sub value = value in *)
(* let%sub max_val = max_val in *)
(* let%arr value = value *)
(* and max_val = max_val in *)
(* Form.both value max_val *)

let handle_update xs =
  let open Olirvu in
  let module E5M2 = Quantization.FP32_to_FP8 (Quant_intf.E5M2) in
  let module E4M3 = Quantization.FP32_to_FP8 (Quant_intf.E4M3) in
  let module E3M4 = Quantization.FP32_to_FP8 (Quant_intf.E3M4) in
  let result =
    [ "E5M2", List.map ~f:(fun fp32 -> E5M2.quantize ~fp32) xs
    ; "E4M3", List.map ~f:(fun fp32 -> E4M3.quantize ~fp32) xs
    ; "E3M4", List.map ~f:(fun fp32 -> E3M4.quantize ~fp32) xs
    ; "VSQ", xs
    ]
  in
  Effect.return (Vega.build_quantized_view result)
;;
