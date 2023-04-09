open! Bonsai
open! Bonsai_web
module Form = Bonsai_web_ui_form

module type Subview = sig
  type t

  val to_spec_name : t -> string
  val form_of_v : t Form.t Computation.t
  val handle_update : t -> unit Effect.t
end

(* module Subview : Subview = struct *)

(* end *)
