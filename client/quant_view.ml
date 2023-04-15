open! Base
open! Bonsai_web
module Form = Bonsai_web_ui_form
module Arr = Owl_base_dense_ndarray.Generic

module Model = struct
  type t = float list [@@deriving sexp, equal]
end

type t =
  | Uniform of Model.t
  | Gaussian of Model.t
[@@deriving typed_variants, sexp, equal]

let to_spec_name _ = "quant_diff_full"

let build_vals xs =
  let open Bonsai.Let_syntax in
  let value = Form.Elements.Number.float [%here] ~default:0. ~step:0.1 () in
  let%sub values = Form.Elements.Multiple.list [%here] value in
  Form.Dynamic.with_default (Value.return xs) values
;;

let num_elem = 22
let nd_to_1d arr = Array.init num_elem ~f:(fun idx -> Arr.(( .%{} ) arr idx))

let form_of_v =
  Form.Typed.Variant.make
    (module struct
      module Typed_variant = Typed_variant

      let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
        = function
        | Uniform ->
          let arr = Arr.sequential Bigarray.Float32 ~a:(-1.0) ~step:0.1 [| num_elem |] in
          build_vals (Array.to_list (nd_to_1d arr))
        | Gaussian ->
          let arr = Arr.gaussian Bigarray.Float32 ~mu:0. ~sigma:(-1.0) [| num_elem |] in
          build_vals (Array.to_list (nd_to_1d arr))
      ;;
    end)
;;

let mse x x_recon =
  let xs = List.zip_exn x x_recon in
  let se = List.fold ~init:0. ~f:(fun acc (x, x_recon) -> (acc +. ((x -. x_recon) **. 2.))) xs in
  (se /. (List.length xs |> Float.of_int))
    
let handle_list xs =
  if List.is_empty xs
  then Effect.Ignore
  else
    let open Olirvu in
    let module E5M2 = Quantization.FP32_to_FP_Q (Quant_intf.E5M2) in
    let module E4M3 = Quantization.FP32_to_FP_Q (Quant_intf.E4M3) in
    let module E3M4 = Quantization.FP32_to_FP_Q (Quant_intf.E3M4) in
    let module INT8 = Quantization.FP32_to_INT_Q (Quant_intf.INT8) in
    let module VSQ = Quantization.FP32_to_VSQ (Quant_intf.VSQ_V16) in
    let e5 = E5M2.quantize ~fp32:xs in
    let e4 = E4M3.quantize ~fp32:xs in
    let e3 = E3M4.quantize ~fp32:xs in
    let i8, i8_recon = INT8.quantize ~fp32:xs |> Base.List.unzip in
    let vsq, vsq_recon = VSQ.quantize ~fp32:xs |> Base.List.unzip in
    let fp_result =
      [ "FP32", xs
      ; "E5M2", e5
      ; "E4M3", e4
      ; "E3M4", e3
      ]
    in
    let int_result = [ "INT8",  i8; "VSQ",  vsq] in
    (* Hist *)
    let hist = Owl_base_stats.histogram (`N 16) (Array.of_list xs) in
    (* MSE *)
    let mses = List.map ~f:(mse xs) [e5; e4; e3; i8_recon; vsq_recon] in
    Brr.Console.(log [ Jv.of_list Jv.of_float mses ]);
    Effect.return (Vega.build_quantized_view hist fp_result int_result)
;;

let handle_update = function
  | Uniform xs -> handle_list xs
  | Gaussian xs -> handle_list xs
;;
