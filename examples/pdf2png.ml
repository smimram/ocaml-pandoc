(** Change the extension of pdf images to png. *)

open Pandoc

let () =
  let p = Pandoc.of_json (Yojson.Basic.from_channel stdin) in
  let f = function
    | Image (a, l, (url, title)) ->
      let url =
        if Filename.check_suffix url ".pdf" then Filename.chop_suffix url ".pdf" ^ ".png"
        else url
      in
      Some [Image (a, l, (url, title))]
    | _ -> None
  in
  let p = Pandoc.map ~inline:f p in
  let s = Yojson.Basic.pretty_to_string (Pandoc.to_json p) in
  print_endline s
