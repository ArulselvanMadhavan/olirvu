open! Base
open! Bonsai_web
module Form = Bonsai_web_ui_form
module Arr = Owl_base_dense_ndarray.Generic

module Model = struct
  type t = int * float list [@@deriving sexp, equal]
end

type t =
  | Uniform of Model.t
  | Gaussian of Model.t
[@@deriving typed_variants, sexp, equal]

let to_spec_name _ = "quant_diff_full"

let tile =
  let value = Form.Elements.Number.float [%here] ~default:0. ~step:0.1 () in
  Form.Elements.Multiple.list [%here] value
;;

let default_tile_size = 8
let tile_size = Form.Elements.Number.int [%here] ~default:default_tile_size ~step:1 ()
let nd_to_1d tsize arr = Array.init tsize ~f:(fun idx -> Arr.(( .%{} ) arr idx))

let uniform_dist num_elem =
  Arr.sequential Bigarray.Float32 ~a:(-10.0) ~step:0.1 [| num_elem |]
  |> nd_to_1d num_elem
  |> Array.to_list
;;

let gaussian_dist num_elem =
  Arr.gaussian Bigarray.Float32 ~mu:0. ~sigma:1.0 [| num_elem |]
  |> nd_to_1d num_elem
  |> Array.to_list
;;

let update_tile tile f =
  let open Bonsai.Let_syntax in
  let%map tile = tile in
  fun tile_size -> Form.set tile (f tile_size)
;;

let build_dist f =
  let open Bonsai.Let_syntax in
  let%sub tsize = tile_size in
  let%sub tile = tile in
  let%sub () = Form.Dynamic.on_change (module Int) ~f:(update_tile tile f) tsize in
  let%arr tsize = tsize
  and tile = tile in
  Form.both tsize tile
;;

let form_of_v =
  Form.Typed.Variant.make
    (module struct
      module Typed_variant = Typed_variant

      let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
        = function
        | Uniform -> build_dist uniform_dist
        | Gaussian -> build_dist gaussian_dist
      ;;
    end)
;;

let sqnr x (name, x_recon) =
  let xs = List.zip_exn x x_recon in
  let sum_sq_errs =
    List.fold ~init:0. ~f:(fun acc (x, x_recon) -> acc +. ((x -. x_recon) **. 2.)) xs
  in
  let mse = sum_sq_errs /. Float.of_int (List.length xs) in
  let sum_sq_signal = List.fold ~init:0. ~f:(fun acc x -> acc +. (x **. 2.)) x in
  let sqnr = 10. *. Float.(log10 (abs (sum_sq_signal /. mse))) in
  name, sqnr
;;

let handle_list (tsize, xs) =
  if List.is_empty xs
  then Effect.Ignore
  else
    let open Olirvu in
    let module E5M2 = Quantization.FP32_to_FP_Q (Quant_intf.E5M2) in
    let module E4M3 = Quantization.FP32_to_FP_Q (Quant_intf.E4M3) in
    let module E3M4 = Quantization.FP32_to_FP_Q (Quant_intf.E3M4) in
    let module INT8 = Quantization.FP32_to_INT_Q (Quant_intf.INT8) in
    let module VSQ = Quantization.FP32_to_VSQ (Quant_intf.VSQ_V128_N8_M4) in
    let e5 = E5M2.quantize ~fp32:xs in
    let e4 = E4M3.quantize ~fp32:xs in
    let e3 = E3M4.quantize ~fp32:xs in
    let i8, i8_recon = INT8.quantize ~fp32:xs |> Base.List.unzip in
    let vsq, vsq_recon = VSQ.quantize ~fp32:xs |> Base.List.unzip in
    let fp_result = [ "FP32", xs; "E5M2", e5; "E4M3", e4; "E3M4", e3 ] in
    let int_result = [ "INT8", i8; "VSQ", vsq ] in
    (* Hist *)
    let bin_count = Int.min tsize 128 in
    let hist = Owl_base_stats.histogram (`N bin_count) (Array.of_list xs) in
    (* SQNR *)
    let sqnrs =
      List.map
        ~f:(sqnr xs)
        [ "E5M2", e5; "E4M3", e4; "E3M4", e3; "INT8", i8_recon; "VSQ", vsq_recon ]
    in
    (* Brr.Console.(log [ Jv.of_list Jv.of_float mses ]); *)
    Effect.return (Vega.build_quantized_view hist fp_result int_result sqnrs)
;;

let handle_update = function
  | Uniform x -> handle_list x
  | Gaussian x -> handle_list x
;;
