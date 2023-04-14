open ATDGenerated

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

let () =
  let open Quantization_t in
  let quant = "FP32" in
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
  let mark =
    { type_ = "group"
    ; name = "child__" ^ quant ^ "_group"
    ; style = "cell"
    ; from = None
    ; encode
    ; signals = Some signals
    ; marks = Some marks
    ; axes = Some [ x_axis_grid; x_axis_labels ]
    }
  in
  let mark_str = Yojson.Safe.prettify (Quantization_j.string_of_marks mark) in
  Printf.printf "%s\n" mark_str
;;
