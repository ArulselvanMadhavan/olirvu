open! Js_of_ocaml

let vega_div = "#viz"

let json_parse s =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "JSON.parse") [| Js.Unsafe.inject (Js.string s) |]
;;

let vega_obj_key = "VEGA_DEBUG"
let await = Fun.flip Fut.await

let vega_embed json_spec =
  let vpromise = Jv.call Jv.global "vegaEmbed" [| Jv.of_string vega_div; json_spec |] in
  let attach_to_global view = Jv.set Jv.global vega_obj_key view in
  Fut.of_promise ~ok:attach_to_global vpromise |> await Result.get_ok;
  Brr.Console.(log [ str "vega_debug attached" ])
;;

let rec edges_list_to_record = function
  | [] -> []
  | (parent, child, elem, rank) :: xs ->
    let parent = if parent = 0 then Jv.null else Jv.of_int parent in
    let color = if Option.is_some elem then "orange" else "silver" in
    let record =
      Jv.obj
        [| "id", Jv.of_int child
         ; "parent", parent
         ; "name", Option.fold ~some:Jv.of_int ~none:(Jv.of_string "E") elem
         ; "rank", Jv.of_int rank
         ; "color", Jv.of_string color
        |]
    in
    record :: edges_list_to_record xs
;;

let update_dataset values =
  let open Jv in
  let open Base in
  let values = Array.of_list values |> of_jv_array in
  let v = find global vega_obj_key |> Option.value_exn in
  let view = find v "view" |> Option.value_exn in
  let vega = find global "vega" |> Option.value_exn in
  let cset = call vega "changeset" [||] in
  let rm = call cset "remove" [| get vega "truthy" |] in
  let ins = call rm "insert" [| values |] in
  let change = call view "change" [| of_string "main"; ins |] in
  let _ = call change "run" [||] in
  ()
;;

let build_heap xs =
  let open Olirvu in
  let module H = Leftist_heap.Make (Base.Int) in
  let h = H.of_list xs in
  let edges = H.edges_list h in
  let records = edges_list_to_record edges in
  update_dataset records;
  Brr.Console.(log [ str "Heap updated" ])
;;

let build_rbt rbt =
  let open Base in
  let open Jv in
  let open Olirvu in
  let rec build_values acc id ~parent = function
    | Rbt.E -> acc, id
    | Rbt.T (c, l, e, r) ->
      let node_id = id + 1 in
      let parent = if parent = 0 then null else of_int parent in
      let record =
        obj
          [| "id", of_int node_id
           ; "parent", parent
           ; "name", of_int e
           ; "color", of_string @@ Rbt.to_string c
          |]
      in
      let acc, id = build_values (record :: acc) node_id ~parent:node_id l in
      build_values acc id ~parent:node_id r
  in
  let values, _ = build_values [] 0 ~parent:0 rbt in
  update_dataset values;
  Brr.Console.(log [ str "RBT updated" ])
;;
