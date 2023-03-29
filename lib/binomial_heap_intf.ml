include Heap_intf

module type Heap = sig
  module Make (T : Base.Comparable.S) : S with module Elem := T
end
