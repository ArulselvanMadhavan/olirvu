module type Quant = sig
  val quantize : fp32:float -> max_val:float -> float
end
