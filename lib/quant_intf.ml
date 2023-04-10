module type Quant = sig
  val quantize : fp32:float -> float
end

module type FP8 = sig
  val n_bits : int
  val mantissa : int
end

module E5M2 : FP8 = struct
  let n_bits = 8
  let mantissa = 2
end

module E4M3 : FP8 = struct
  let n_bits = 8
  let mantissa = 3
end

module E3M4 : FP8 = struct
  let n_bits = 8
  let mantissa = 4
end

module type VSQ = sig
  include Quant
end

module type Builder = sig
  module FP32_to_FP8 (_ : FP8) : Quant
end
