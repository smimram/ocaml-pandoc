(** Remove LaTeX-style comments (lines beginning with %). *)

open Pandoc

let () =
  let p = Pandoc.of_json (Yojson.Basic.from_channel stdin) in
  let f = function
    | Str "%"::_ -> []
    | l -> l
  in
  let p = Pandoc.map_inlines f p in
  let s = Yojson.Basic.pretty_to_string (Pandoc.to_json p) in
  print_endline s
