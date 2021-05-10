(** Replace a word by another in text. *)

open Pandoc

module String = struct
  include String

  let find p s =
    let ans = ref (-1) in
    try
      for i = 0 to String.length s - 1 do
        if p s.[i] then (ans := i; raise Exit)
      done;
      raise Not_found
    with Exit -> !ans

  let split_at i s =
    String.sub s 0 i, String.sub s i (String.length s - i)

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

let re_smallcaps = Str.regexp "^\\[\\(.*\\)\\]{\\.smallcaps}\\([.:,;()]?\\)$"

let () =
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
          (* Remove trailing punctuation. *)
          let s, trail =
            try
              let n = String.find (fun c -> List.mem c ['.'; ':'; ','; ';'; '('; ')']) s in
              String.split_at n s
            with Not_found -> s, ""
          in
          List.assoc s replacements ^ trail
        with Not_found -> s
      in
      let s =
        if Str.string_match re_smallcaps s 0 then
          (* Handle smallcaps. *)
          let str = Str.matched_group 1 s in
          let trail =
            try [Str (Str.matched_group 2 s)]
            with Not_found -> []
          in
          (SmallCaps [Str str])::trail
        else [Str s]
      in
      Some s
    | _ -> None
  in
  let p = Pandoc.map ~block ~inline p in
  let s = Yojson.Basic.pretty_to_string (Pandoc.to_json p) in
  print_endline s
