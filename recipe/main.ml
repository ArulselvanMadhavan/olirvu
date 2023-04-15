open ATDGenerated

let v5_schema = "https://vega.github.io/schema/vega/v5.json"

let build_sample_data quant =
  let open Quantization_t in
  { name = "data_" ^ quant; values = [ { value = 0.5; type_ = quant } ] }
;;

let child_marks quant =
  let open Quantization_t in
  let name = "child__" ^ quant ^ "_marks" in
  let style = "point" in
  let data = Some { data = "data_" ^ quant } in
  let encode =
    { update =
        { width = None
        ; height = None
        ; opacity = Some { value = 0.7 }
        ; fill = Some { signal = "point_color" }
        ; aria_role_description = Some { value = "point" }
        ; x = Some { scale = "child__" ^ quant ^ "_x"; field = "value" }
        ; y = Some { signal = "childHeight"; mult = 0.5 }
        ; tooltip = Some [ { signal = "{\"value\":datum[\"value\"" } ]
        }
    }
  in
  { type_ = "symbol"
  ; name
  ; style
  ; from = data
  ; encode
  ; signals = None
  ; marks = None
  ; axes = None
  }
;;

let supported_quants = [ "FP32"; "E5M2"; "E4M3"; "E3M4"; "VSQ" ]

let build_mark quant =
  let open Quantization_t in
  let width = Some { signal = "childWidth" } in
  let height = Some { signal = "childHeight" } in
  let encode =
    { update =
        { width
        ; height
        ; opacity = None
        ; fill = None
        ; aria_role_description = None
        ; x = None
        ; y = None
        ; tooltip = None
        }
    }
  in
  let signals = [ { name = "height"; update = "childHeight" } ] in
  let marks = [ child_marks quant ] in
  let x_axis_grid =
    { scale = "child__" ^ quant ^ "_x"
    ; orient = "bottom"
    ; grid = true
    ; tick_count = { signal = "ceil(childWidth/40)" }
    ; domain = false
    ; labels = false
    ; aria = false
    ; max_extent = 0
    ; min_extent = 0
    ; ticks = false
    ; zindex = 0
    ; label_flush = None
    ; label_overlap = None
    ; title = None
    }
  in
  let x_axis_labels =
    { x_axis_grid with
      grid = false
    ; label_flush = Some true
    ; label_overlap = Some true
    ; title = Some quant
    }
  in
  { type_ = "group"
  ; name = "child__" ^ quant ^ "_group"
  ; style = "cell"
  ; from = None
  ; encode
  ; signals = Some signals
  ; marks = Some marks
  ; axes = Some [ x_axis_grid; x_axis_labels ]
  }
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

let () =
  let open Quantization_t in
  let data = Base.List.map supported_quants ~f:build_sample_data in
  let marks = Base.List.map supported_quants ~f:build_mark in
  let signals =
    [ { name = "childWidth"; value = `Int 200 }
    ; { name = "childHeight"; value = `Int 20 }
    ; { name = "point_color"; value = `String "#4778a8" }
    ]
  in
  let layout = { padding = 20; columns = 2; bounds = "full"; align = "all" } in
  let scales = Base.List.map supported_quants ~f:build_scale in
  let recipe =
    { schema = v5_schema
    ; background = "white"
    ; padding = 5
    ; marks
    ; data
    ; signals
    ; layout
    ; scales
    }
  in
  Stdio.Out_channel.write_all
    "quant_diff_full_generated.vg.json"
    ~data:(Yojson.Safe.prettify (Quantization_j.string_of_recipe recipe))
;;
