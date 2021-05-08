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
let replacements =
  let fname = "replacements" in
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

let () =
  let p = Pandoc.of_json (Yojson.Basic.from_channel stdin) in
  let f = function
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
      Some [Str s]
    | _ -> None
  in
  let p = Pandoc.map ~inline:f p in
  let s = Yojson.Basic.pretty_to_string (Pandoc.to_json p) in
  print_endline s
