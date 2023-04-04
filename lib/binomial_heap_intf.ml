include Heap_intf

module type Builder = sig
  module Make (T : Base.Comparable.S) : Heap with module Elem := T
end
