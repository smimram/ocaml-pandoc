(** Pandoc extension to include files. *)

let () =
  let p = Pandoc.of_json (Yojson.Basic.from_channel stdin) in
  let rec f = function
    (* !include "file" *)
    | Pandoc.Para [Str "!include"; _; Quoted (DoubleQuote, [Str s])] ->
      let p = Pandoc.of_md_file s in
      let p = Pandoc.map_blocks f p in
      Some p.blocks
    (* ```{.blabla include="file"}
       ``` *)
    | CodeBlock ((ident, classes, keyvals), _) when List.mem_assoc "include" keyvals ->
      let fname = List.assoc "include" keyvals in
      let contents =
        if not (Sys.file_exists fname) then
          let err = "FILE NOT FOUND: " ^ fname in
          Printf.eprintf "%s\n%!" err;
          err
        else
          let nb_lines () =
            let ans = ref 0 in
            let ic = open_in fname in
            try
              while true do
                ignore (input_line ic);
                incr ans
              done;
              0
            with
            | End_of_file -> !ans
          in
          let from = try int_of_string (List.assoc "from" keyvals) with Not_found -> 0 in
          let last = try int_of_string (List.assoc "to" keyvals) with Not_found -> max_int in
          let from = if from < 0 then nb_lines () + from else from in
          let last = if last < 0 then nb_lines () - 1 + last else last in

          try
            let ic = open_in fname in
            let ans = ref "" in
            let line = ref 0 in
            try
              while true do
                let l = input_line ic in
                if !line >= from && !line <= last then ans := !ans ^ l ^ "\n";
                incr line
              done;
              ""
            with
            | End_of_file -> !ans
          with
          | Sys_error _ ->
            let err = "ERROR: file \""^fname^"\" not found!" in
            prerr_endline err;
            err
      in
      Some [Pandoc.CodeBlock ((ident, classes, keyvals), contents)]
    | b -> None
  in
  let p = Pandoc.map_blocks f p in
  let s = Yojson.Basic.pretty_to_string (Pandoc.to_json p) in
  print_endline s
