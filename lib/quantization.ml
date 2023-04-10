include Quant_intf

module FP32_to_FP8 (F : FP8) : Quant = struct
  let n_bits = F.n_bits
  let mantissa = F.mantissa
  let exponent = n_bits - 1 - mantissa
  let exp_minus_1 = exponent - 1
  let bias_offset = Base.Int.((2 ** exp_minus_1) - 1)
  let mant_rem = 23 - mantissa

  let target_exp tgt =
    let open Unsigned.UInt32 in
    let open Infix in
    let tgt = of_int32 tgt in
    let te = ((tgt lsl 1) lsr 1) lsr 23 in
    to_int32 (te - of_int bias_offset)
  ;;

  let min_exp =
    let me = -((1 lsl exp_minus_1) - 2) in
    Base.Int32.of_int_exn me
  ;;

  let round_bitwise target =
    (* Nearest rounding *)
    let m_rem = mant_rem - 1 in
    let open Unsigned.UInt32 in
    let open Infix in
    let one = of_int 1 in
    let mask = (one lsl mant_rem) - one in
    let rand_prob = of_int 1 lsl m_rem in
    let add_r = add target rand_prob in
    logand add_r (lognot mask)
  ;;

  let clip_exponent old_num q_num =
    let open Unsigned.UInt32 in
    let open Infix in
    let clip_up max_exp =
      let max_man = (((of_int (-1) lsl 9) lsr 9) lsr mant_rem) lsl mant_rem in
      let max_num = (max_exp lsl 23) lor max_man in
      let old_sign = (old_num lsr 31) lsl 31 in
      old_sign lor max_num
    in
    let clip_quant () =
      let q_exp_store = ((q_num lsl 1) lsr 1) lsr 23 |> to_int32 in
      let max_exp_store = (of_int 1 lsl exp_minus_1) + of_int 127 in
      if q_exp_store > to_int32 max_exp_store then clip_up max_exp_store else q_num
    in
    if equal q_num zero then q_num else clip_quant ()
  ;;

  let quantize_normal target =
    let open Unsigned.UInt32 in
    let target = of_int32 target in
    let quantized_bits = round_bitwise target in
    clip_exponent target quantized_bits |> Unsigned.UInt32.to_int32
  ;;

  let quantize ~fp32 =
    let open Base.Int32 in
    let target = bits_of_float fp32 in
    let target_exp = target_exp target in
    let is_subnormal = target_exp < min_exp in
    if is_subnormal then fp32 else quantize_normal target |> float_of_bits
  ;;
end
