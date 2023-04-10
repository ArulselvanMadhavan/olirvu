module type Quant = sig
  val quantize : fp32:float -> float
end

module type FP8 = sig
  val n_bits : int
  val mantissa : int
  (* val exponent : int *)
  (* val bias_offset : int *)

  include Quant
end

module type VSQ = sig
  include Quant
end
