(** Pandoc extension to include files. *)

module String = struct
  include String

  (* TODO: more efficient version *)
  let contains_substring l s =
    let n = String.length s in
    try
      for i = 0 to String.length l - n do
        if String.sub l i n = s then raise Exit
      done;
      false
    with
    | Exit -> true
end

let error fname n =
  Printf.ksprintf
    (fun s ->
       Printf.eprintf "pandoc-include error in file `%s`: %s\n%!" fname s;
       exit n)

let () =
  let directory = ref "" in
  Arg.parse
    [
      "--directory", Arg.Set_string directory, "Use this as base directory for included files."
    ]
    ignore
    "pandoc-include [options]";
  let p = Pandoc.of_json (Yojson.Basic.from_channel stdin) in
  let rec f = function
    (* !include "file" *)
    | Pandoc.Para [Str "!include"; _; Quoted (DoubleQuote, [Str fname])] ->
      let fname = Filename.concat !directory fname in
      let p = Pandoc.of_md_file fname in
      let p = Pandoc.map_blocks f p in
      Some (Pandoc.blocks p)
    (* ```{.blabla include="file"}
       ``` *)
    | CodeBlock ((ident, classes, keyvals), _) when List.mem_assoc "include" keyvals ->
      let fname = List.assoc "include" keyvals |> Filename.concat !directory in
      let error n = error fname n in
      let from =
        match List.assoc_opt "from" keyvals with
        | None -> `Int 0
        | Some from ->
            match int_of_string_opt from with
            | Some from -> `Int from
            | None -> `String from
      in
      let last =
        match List.assoc_opt "to" keyvals with
        | None -> `Last
        | Some last ->
          match int_of_string_opt last with
          | Some last -> `Int last
          | None -> `String last
      in
      let contents =
        if not (Sys.file_exists fname) then
          error 1 "Could not find file."
        else
          let from = ref from in
          let last = ref last in
          let lines = ref 0 in
          (
            let ic = open_in fname in
            try
              while true do
                let l = input_line ic in
                (
                  match !from with
                  | `String s ->
                    if String.contains_substring l s then from := `Int (!lines + 1)
                  | _ -> ()
                );
                (
                  match !last with
                  | `String s ->
                    if String.contains_substring l s then last := `Int (!lines - 1)
                  | _ -> ()
                );
                incr lines
              done
            with
            | End_of_file -> close_in ic
          );
          let lines = !lines in
          let from =
            match !from with
            | `Int n -> n
            | `String s -> error 2 "Could not find string `%s` for first line." s
          in
          let last =
            match !last with
            | `Int n -> n
            | `Last -> lines - 1
            | `String s -> error 2 "Could not find string `%s` for last line." s
          in
          let from = if from < 0 then lines + from else from in
          let last = if last < 0 then lines - 1 + last else last in
          if from < 0 || from >= lines then error 3 "First line (%d) out of range." from;
          if last < 0 || last >= lines then error 3 "Last line (%d) out of range." last;
          try
            let ic = open_in fname in
            let ans = ref "" in
            let line = ref 0 in
            try
              while true do
                let l = input_line ic in
                if !line > last then raise Exit;
                if !line >= from then ans := !ans ^ l ^ "\n";
                incr line
              done;
              ""
            with
            | End_of_file | Exit -> !ans
          with
          | Sys_error _ as err ->
            error 1 "System error: %s." (Printexc.to_string err);
      in
      Some [Pandoc.CodeBlock ((ident, classes, keyvals), contents)]
    | _ -> None
  in
  let p = Pandoc.map_blocks f p in
  let s = Yojson.Basic.pretty_to_string (Pandoc.to_json p) in
  print_endline s
