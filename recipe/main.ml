open ATDGenerated
    
let () =
  (* Yojson.Safe.prettify *)
  let open Quantization_t in
  let mark = {type_ = "group"} in
  let mark_str = Yojson.Safe.prettify (Quantization_j.string_of_marks mark) in
  Printf.printf "%s\n" mark_str
