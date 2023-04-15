module type Quant = sig
  type t

  val quantize : fp32:float list -> t list
end

module type FP_Q = sig
  type t = float

  val n_bits : int
  val mantissa : int
end

module E5M2 : FP_Q = struct
  type t = float

  let n_bits = 8
  let mantissa = 2
end

module E4M3 : FP_Q = struct
  type t = float

  let n_bits = 8
  let mantissa = 3
end

module E3M4 : FP_Q = struct
  type t = float

  let n_bits = 8
  let mantissa = 4
end

module type INT_Q = sig
  type t = int * float

  val n_bits : int
end

module INT8 : INT_Q = struct
  type t = int * float

  let n_bits = 8
end

module type VSQ = sig
  type t = int * float

  val n_bits : int
  val m_bits : int
  val tile_size : int
end

module VSQ_V16 : VSQ = struct
  type t = int * float

  let n_bits = 4
  let m_bits = 4
  let tile_size = 16
end

module type Builder = sig
  module FP32_to_FP_Q (F : FP_Q) : Quant with type t := F.t
  module FP32_to_INT_Q (I : INT_Q) : Quant with type t := I.t
  module FP32_to_VSQ (V : VSQ) : Quant with type t := V.t
end
