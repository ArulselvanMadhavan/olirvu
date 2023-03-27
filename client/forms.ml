open Base
module Form = Bonsai_web_ui_form

module RBT_Model = struct
  type t = int list [@@deriving sexp, equal]
end

let rbtree_form =
  let open! Bonsai.Let_syntax in
  let int_form = Form.Elements.Number.int [%here] ~default:0 ~step:1 () in
  Form.Elements.Multiple.list [%here] int_form
;;

(* let heap_form = *)
(*   let int_form = Form.Elements.Number in [%here] ~default:0 ~step:1 () in *)
(*   Form.Elements.Multiple.list [%here] int_form *)
