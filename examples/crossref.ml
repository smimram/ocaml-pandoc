(** Pandoc extension to have cross-references in LaTeX. Replaces links of the
    form #sec:blabla with a \cref{sec:blabla}. *)

let begins_with prefix s =
  let l = String.length prefix in
  String.length s >= l && String.sub s 0 l = prefix

let () =
  let p = Pandoc.of_json (Yojson.Basic.from_channel stdin) in
  let f = function
    | Pandoc.Link (_, _, (url, _)) ->
       if not (begins_with "#chap:" url || begins_with "#sec:" url) then None
       else
         let link = String.sub url 1 (String.length url - 1) in
         Some [Pandoc.RawInline ("tex", Printf.sprintf "\\cref{%s}" link)]
    | _ -> None
  in
  let p = Pandoc.map ~inline:f p in
  let s = Yojson.Basic.pretty_to_string (Pandoc.to_json p) in
  print_endline s
