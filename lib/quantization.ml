include Quant_intf

module QPT_FP8 : Quant = struct
  let quantize ~fp32 ~max_val =
    let _m = max_val in
    fp32
end

