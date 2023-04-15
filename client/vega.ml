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

let build_record child parent elem rank color =
  Jv.obj
    [| "id", Jv.of_int child
     ; "parent", parent
     ; "name", elem
     ; "rank", Jv.of_int rank
     ; "color", Jv.of_string color
    |]
;;

let rec edges_list_to_record ~parent_is_null = function
  | [] -> []
  | (parent, child, elem, rank) :: xs ->
    let parent = if parent = 0 && parent_is_null then Jv.null else Jv.of_int parent in
    let color = if Option.is_some elem then "orange" else "silver" in
    let elem = Option.fold ~some:Jv.of_int ~none:(Jv.of_string "E") elem in
    let record = build_record child parent elem rank color in
    record :: edges_list_to_record xs ~parent_is_null
;;

let update_dataset ?(name = "main") values =
  let open Jv in
  let open Base in
  let values = Array.of_list values |> of_jv_array in
  let v = find global vega_obj_key |> Option.value_exn in
  let view = find v "view" |> Option.value_exn in
  let vega = find global "vega" |> Option.value_exn in
  let cset = call vega "changeset" [||] in
  let rm = call cset "remove" [| get vega "truthy" |] in
  let ins = call rm "insert" [| values |] in
  let change = call view "change" [| of_string name; ins |] in
  let _ = call change "run" [||] in
  ()
;;

let build_heap xs =
  let open Olirvu in
  let module H = Leftist_heap.Make (Base.Int) in
  let h = H.of_list xs in
  let edges = H.edges_list h in
  let records = edges_list_to_record edges ~parent_is_null:true in
  update_dataset records;
  Brr.Console.(log [ str "Heap updated" ])
;;

let build_bin_heap xs =
  let open Olirvu in
  let module H = Binomial_heap.Make (Base.Int) in
  let h = H.of_list xs in
  let edges = H.edges_list h in
  let records = edges_list_to_record edges ~parent_is_null:false in
  let parent_record = build_record 0 Jv.null (Jv.of_string "heap") (-1) "silver" in
  update_dataset (parent_record :: records);
  Brr.Console.(log [ str "Bin heap updated"; records ])
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

let build_coin_change xs =
  let open Base in
  let open Jv in
  let values =
    Array.mapi xs ~f:(fun amount num_coins ->
      obj
        [| "amount", of_int amount
         ; "num_coins", of_int (Option.value num_coins ~default:(-1))
        |])
  in
  let values = Array.to_list values in
  update_dataset values;
  Brr.Console.(log [ str "Coin change updated" ])
;;

let build_quantized_view hist fp_xs int_xs vsq_xs =
  let open Jv in
  let update_data xs val_func =
    List.iter
      (fun (type_, qvalues) ->
        let values =
          List.map
            (fun value -> obj [| "type_", of_string type_; "value", val_func value |])
            qvalues
        in
        update_dataset ~name:("data_" ^ type_) values)
      xs
  in
  update_data fp_xs of_float;
  update_data int_xs of_int;
  update_data vsq_xs (fun (x, _) -> of_int x);
  let open Owl_base_stats in
  let gen_hist_datum idx count =
    obj [|"bin_start", of_float hist.bins.(idx); "bin_end", of_float hist.bins.(idx + 1); "count", of_int count|]
  in
  let values = Array.mapi gen_hist_datum hist.counts |> Array.to_list in
  Brr.Console.(log [values]);
  update_dataset ~name:("hist_source_0") values
;;
