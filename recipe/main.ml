open ATDGenerated

let v5_schema = "https://vega.github.io/schema/vega/v5.json"

let build_sample_data quant =
  let open Quantization_t in
  let datum = `Assoc [ "value", `Float 0.5; "type_", `String quant] in
  let data = [datum] in
  { name = "data_" ^ quant; values = data }
;;

let child_marks quant =
  let open Quantization_t in
  let name = "child__" ^ quant ^ "_marks" in
  let style = "point" in
  let data = { data = "data_" ^ quant } in
  let encode =
    { update =
        make_update
          ~opacity:{ value = 0.7 }
          ~fill:{ signal = "point_color" }
          ~aria_role_description:{ value = "point" }
          ~x:{ scale = "child__" ^ quant ^ "_x"; field = "value" }
          ~y:{ signal = "childHeight"; mult = 0.5 }
          ~tooltip:[ { signal = "{\"value\":datum[\"value\"]}" } ]
          ()
    }
  in
  make_marks ~type_:"symbol" ~name ~style ~from:data ~encode ()
;;

let supported_quants = [ "FP32"; "E5M2"; "E4M3"; "E3M4"; "INT8"; "VSQ" ]

let build_mark quant =
  let open Quantization_t in
  let width = { signal = "childWidth" } in
  let height = { signal = "childHeight" } in
  let encode = { update = make_update ~width ~height () } in
  let signals =
    [ `Assoc [ "name", `String "height"; "update", `String "childHeight" ] ]
  in
  let marks = [ child_marks quant ] in
  let scale = "child__" ^ quant ^ "_x" in
  let orient = "bottom" in
  let tick_count = { signal = "ceil(childWidth/40)" } in
  let x_axis_grid =
    make_axis
      ~scale
      ~orient
      ~grid:true
      ~tick_count
      ~domain:false
      ~labels:false
      ~aria:false
      ~max_extent:0
      ~min_extent:0
      ~ticks:false
      ~zindex:0
      ()
  in
  let x_axis_labels =
    make_axis
      ~scale
      ~orient
      ~grid:false
      ~title:quant
      ~label_flush:true
      ~label_overlap:true
      ~tick_count
      ~zindex:0
      ()
  in
  make_marks
    ~type_:"group"
    ~name:("child__" ^ quant ^ "_group")
    ~style:"cell"
    ~encode
    ~signals
    ~marks
    ~axes:[ x_axis_grid; x_axis_labels ]
    ()
;;

let build_scale quant =
  let open Quantization_t in
  { name = "child__" ^ quant ^ "_x"
  ; type_ = "linear"
  ; domain = { data = "data_" ^ quant; field = "value" }
  ; range = [ `Int 0; `Assoc [ "signal", `String "childWidth" ] ]
  ; nice = true
  ; zero = true
  }
;;

let scale_group () =
  let open Quantization_t in
  let data = Base.List.map supported_quants ~f:build_sample_data in
  let marks = Base.List.map supported_quants ~f:build_mark in
  let signals =
    [ `Assoc [ "name", `String "childWidth"; "value", `Int 200 ]
    ; `Assoc [ "name", `String "childHeight"; "value", `Int 20 ]
    ; `Assoc [ "name", `String "point_color"; "value", `String "#4778a8" ]
    ]
  in
  let layout = make_layout ~padding:20 ~columns:2 ~bounds:"full" ~align:"all" () in
  let scales = Base.List.map supported_quants ~f:build_scale in
  make_marks
    ~type_:"group"
    ~name:"scale_group"
    ~style:"cell"
    ~data
    ~signals
    ~layout
    ~scales
    ~marks
    ()
;;

(* let hist_group () = *)
(*   let open Quantization_t in *)
(*   let values =  *)
(*   let data = make_data_values ~name:"source_0" ~values () in *)
(*   make_marks ~type_:"group" ~name:"hist_group" ~style:"cell"  *)
let () =
  let open Quantization_t in
  let layout = make_layout ~padding:50 ~columns:1 () in
  let marks = [ scale_group () ] in
  let recipe = make_recipe ~schema:v5_schema ~background:"white" ~marks ~layout () in
  Stdio.Out_channel.write_all
    "recipe/quant_diff_full_generated.vg.json"
    ~data:(Yojson.Safe.prettify (Quantization_j.string_of_recipe recipe))
;;
