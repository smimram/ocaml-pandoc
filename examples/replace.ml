(** Replace a word by another in text. *)

open Pandoc

module String = struct
  include String

  let split_on_first_char c s =
    try
      let n = String.index s c in
      Some (String.sub s 0 n, String.sub s (n+1) (String.length s - (n+1)))
    with Not_found -> None
end

(** List of replacements to perform. *)
let replacements fname =
  let ic = open_in fname in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  let s = Bytes.unsafe_to_string s in
  let l = String.split_on_char '\n' s in
  let l = List.filter_map (String.split_on_first_char ' ') l in
  l

(*
let () =
  let a = abbreviations |> List.map (fun (a,b) -> a ^ " -> " ^ b) |> String.concat "\n" in
  prerr_endline ("Replacements:\n" ^ a ^ "\n")
*)

let re_word = Str.regexp "^\\([.:,;()]*\\)\\([a-zA-Z0-9]+\\)\\([.:,;()]*\\)$"
let re_smallcaps = Str.regexp "^\\([.:,;()]*\\)\\[\\(.*\\)\\]{\\.smallcaps}\\([.:,;()]*\\)$"

let () =
  Printexc.record_backtrace true;
  let p = Pandoc.of_json (Yojson.Basic.from_channel stdin) in
  (* File with all replacements. *)
  let fname =
    try Pandoc.meta_string p "replacements"
    with Not_found -> "replacements"
  in
  (* Should we replace in headers? *)
  let replace_headers =
    try Pandoc.meta_bool p "replace-headers"
    with Not_found -> true
  in
  let replacements = replacements fname in
  let block = function
    | Header _ as b when not replace_headers -> Some [b]
    | _ -> None
  in
  let inline = function
    | Str s ->
      let s =
        try
          if Str.string_match re_word s 0 then
            let pre = Str.matched_group 1 s in
            let post = Str.matched_group 3 s in
            let s = Str.matched_group 2 s in
            pre ^ List.assoc s replacements ^ post
          else
            List.assoc s replacements
        with
        | Not_found -> s
      in
      let s =
        if Str.string_match re_smallcaps s 0 then
          (* Handle smallcaps. *)
          let pre = Str.matched_group 1 s in
          let post = Str.matched_group 3 s in
          let s = Str.matched_group 2 s in
          List.filter (fun x -> x <> Str "") [Str pre; SmallCaps [Str s]; Str post]
        else [Str s]
      in
      Some s
    | _ -> None
  in
  let p = Pandoc.map ~block ~inline p in
  let s = Yojson.Basic.pretty_to_string (Pandoc.to_json p) in
  print_endline s
