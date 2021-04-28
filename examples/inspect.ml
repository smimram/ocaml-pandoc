(** Simple pandoc extension which prints JSON representation on standard
    error. *)

let () =
  let json = Yojson.Basic.from_channel stdin in
  let s = Yojson.Basic.pretty_to_string json in
  prerr_endline s;
  print_endline s
