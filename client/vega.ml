open Js_of_ocaml
open Base

let vega_embed json_spec =
  (* Call vegaEmbed *)
  (* Promise resolves to view *)
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "vegaEmbed")
    [| Js.Unsafe.inject (Js.string "#viz"); json_spec |]
;;

let view_insert _xs =
  let open Option.Let_syntax in
  let result () = 
  let%bind v = Jv.find Jv.global "VEGA_DEBUG" in
  let v = Jv.find v "view" |> Option.value_exn in
  let data = Jv.(call v "data" [| of_string "tree" |]) in
  (* Js_of_ocaml.Json.output "arul" *)
  let console = Jv.find Jv.global "console" |> Option.value_exn in
  let _ = Jv.call console "log" [| data |] in
  Some ()
in result () |> Option.fold ~init:() ~f:(fun _ _ -> ())

;;
(* Js.Unsafe.fun_call (Js.Unsafe.js_expr "VEGA_DEBUG") *)
