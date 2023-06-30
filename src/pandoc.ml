open Yojson.Basic
open Util

(* https://hackage.haskell.org/package/pandoc-types-1.19/docs/Text-Pandoc-Definition.html
   https://hackage.haskell.org/package/pandoc-types-1.23/docs/Text-Pandoc-Definition.html
   https://github.com/jgm/pandoc-types/blob/master/changelog *)

(** The version we are currently handling. *)
(* TODO: get rid of this global reference at some point *)
let current_version = ref [1;22]

let compare_version v1 v2 =
  let rec aux v1 v2 =
    match (v1,v2) with
    | m::v1, n::v2 ->
      if m = n then aux v1 v2
      else if m < n then -1
      else 1
    | [], _::_ -> -1
    | _::_, [] -> 1
    | [], [] -> 0
  in
  aux v1 v2

(* Test whether current version is greater or equal to given version. *)
let version_ge v = compare_version !current_version v >= 0

type format = string

(** Attributes: identifier, classes, key/values. *)
type attr = string * string list * (string * string) list

(** Target: url, title. *)
type target = string * string

type list_number_style = DefaultStyle | Example | Decimal | LowerRoman | UpperRoman | LowerAlpha | UpperAlpha

type list_number_delim = DefaultDelim | Period | OneParen | TwoParensPeriod

type list_attributes = int * list_number_style * list_number_delim

type quote_type = DoubleQuote | SingleQuote

type math_type = DisplayMath | InlineMath

type citation_mode = AuthorInText | SuppressAuthor | NormalCitation

type alignment = AlignLeft | AlignRight | AlignCenter | AlignDefault

type col_width = ColWidth of float | ColWidthDefault

type col_spec = alignment * col_width

type row_span = RowSpan of int

type col_span = ColSpan of int

type row_head_columns = RowHeadColumns of int

type inline =
  | Code of attr * string
  | Emph of inline list
  | Image of attr * inline list * target
  | Link of attr * inline list * target
  | Quoted of quote_type * inline list
  | RawInline of string * string
  | Space
  | SmallCaps of inline list
  | Str of string
  | Underline of inline list
  | Strong of inline list
  | Strikeout of inline list
  | Superscript of inline list
  | Subscript of inline list
  | Cite of citation list * inline list
  | SoftBreak
  | LineBreak
  | Math of math_type * string
  | Note of block list
  | Span of attr * inline list
  | UnhandledInline of Yojson.Basic.t

and block =
  | BulletList of block list list
  | CodeBlock of attr * string
  | Header of int * attr * inline list
  | OrderedList of list_attributes * block list list
  | Para of inline list
  | Plain of inline list
  | RawBlock of format * string
  | Div of attr * block list
  | LineBlock of inline list list
  | BlockQuote of block list
  | DefinitionList of (inline list * block list list) list
  | HorizontalRule
  | Table of attr * caption * col_spec list * table_head * table_body list * table_foot
  | Figure of attr * caption * block list
  | UnhandledBlock of Yojson.Basic.t

and citation =
  Citation of
    { citation_id       : string
    ; citation_prefix   : inline list
    ; citation_suffix   : inline list
    ; citation_mode     : citation_mode
    ; citation_note_num : int
    ; citation_hash     : int
    }

and caption = Caption of short_caption option * block list

and short_caption = inline list

and cell = Cell of attr * alignment * row_span * col_span * block list

and row = Row of attr * cell list

and table_head = TableHead of attr * row list

and table_body = TableBody of attr * row_head_columns * row list * row list

and table_foot = TableFoot of attr * row list

type t = { api_version : int list; meta : Yojson.Basic.t; blocks : block list }

module JSON = struct
  let element_type e =
    Util.to_string (List.assoc "t" (to_assoc e))

  let element_contents e =
    List.assoc "c" (to_assoc e)

  let to_pair p =
    match Util.to_list p with
    | [x; y] -> x, y
    | _ -> assert false

  let to_triple p =
    match Util.to_list p with
    | [x; y; z] -> x, y, z
    | _ -> assert false

  let to_quadruple p =
    match Util.to_list p with
    | [w; x; y; z] -> w, x, y, z
    | _ -> assert false

  let to_quintuple p =
    match Util.to_list p with
    | [v; w; x; y; z] -> v, w, x, y, z
    | _ -> assert false

  let to_sextuple p =
    match Util.to_list p with
    | [u; v; w; x; y; z] -> u, v, w, x, y, z
    | _ -> assert false

  let to_attr attr =
    let id, classes, keyvals = to_triple attr in
    let id = Util.to_string id in
    let classes = List.map Util.to_string (Util.to_list classes) in
    let keyvals = List.map Util.to_list (Util.to_list keyvals) in
    let keyvals = List.map (function [k;v] -> (Util.to_string k, Util.to_string v) | _ -> assert false) keyvals in
    id, classes, keyvals

  let to_target t =
    let url, title = to_pair t in
    Util.to_string url, Util.to_string title

  let to_list_attributes a =
    let n, ns, nd = to_triple a in
    let n = Util.to_int n in
    let ns =
      match element_type ns with
      | "DefaultStyle" -> DefaultStyle
      | "Example" -> Example
      | "Decimal" -> Decimal
      | "LowerRoman" -> LowerRoman
      | "UpperRoman" -> UpperRoman
      | "LowerAlpha" -> LowerAlpha
      | "UpperAlpha" -> UpperAlpha
      | _ -> assert false
    in
    let nd =
      match element_type nd with
      | "DefaultDelim" -> DefaultDelim
      | "Period" -> Period
      | "OneParen" -> OneParen
      | "TwoParensPeriod" -> TwoParensPeriod
      | _ -> assert false
      in
    n, ns, nd

  let to_math_type t =
    match element_type t with
    | "DisplayMath" -> DisplayMath
    | "InlineMath" -> InlineMath
    | _ -> assert false

  let to_alignment a =
    match element_type a with
    | "AlignLeft" -> AlignLeft
    | "AlignRight" -> AlignRight
    | "AlignCenter" -> AlignCenter
    | "AlignDefault" -> AlignDefault
    | _ -> assert false

  let to_citation_mode m =
    match element_type m with
    | "AuthorInText" -> AuthorInText
    | "SuppressAuthor" -> SuppressAuthor
    | "NormalCitation" -> NormalCitation
    | _ -> assert false

  let rec to_inline e =
    match element_type e with
    | "Code" ->
      let a, c = to_pair (element_contents e) in
      Code (to_attr a, Util.to_string c)
    | "Emph" ->
      Emph (to_inline_list (element_contents e))
    | "Image" ->
      let a, i, t = to_triple (element_contents e) in
      Image
        ( to_attr a
        , to_inline_list i
        , to_target t
        )
    | "Link" ->
      let a, i, t = to_triple (element_contents e) in
      Link
        ( to_attr a
        , to_inline_list i
        , to_target t
        )
    | "Quoted" ->
      let q, l = to_pair (element_contents e) in
      let q =
        match element_type q with
        | "DoubleQuote" -> DoubleQuote
        | "SingleQuote" -> SingleQuote
        | q -> failwith ("Unhandled quote type " ^ q)
      in
      Quoted (q, to_inline_list l)
    | "RawInline" ->
      let fmt, contents = to_pair (element_contents e) in
      RawInline (Util.to_string fmt, Util.to_string contents)
    | "Space" ->
      Space
    | "SmallCaps" ->
      SmallCaps (to_inline_list (element_contents e))
    | "Str" ->
      Str (Util.to_string (element_contents e))
    | "Underline" ->
      Underline (to_inline_list (element_contents e))
    | "Strong" ->
      Strong (to_inline_list (element_contents e))
    | "Strikeout" ->
      Strikeout (to_inline_list (element_contents e))
    | "Superscript" ->
      Superscript (to_inline_list (element_contents e))
    | "Subscript" ->
      Subscript (to_inline_list (element_contents e))
    | "Cite" ->
      let c, i = to_pair (element_contents e) in
      let to_citation x =
        Citation
          { citation_id = Util.to_string (Util.member "citationId" x)
          ; citation_prefix = to_inline_list (Util.member "citationPrefix" x)
          ; citation_suffix = to_inline_list (Util.member "citationSuffix" x)
          ; citation_mode = to_citation_mode (Util.member "citationMode" x)
          ; citation_note_num = Util.to_int (Util.member "citationNoteNum" x)
          ; citation_hash = Util.to_int (Util.member "citationHash" x)
          }
      in
      Cite (List.map to_citation (Util.to_list c), to_inline_list i)
    | "SoftBreak" ->
      SoftBreak
    | "LineBreak" ->
      LineBreak
    | "Math" ->
      let t, m = to_pair (element_contents e) in
      Math (to_math_type t, Util.to_string m)
    | "Note" ->
      Note (to_block_list (element_contents e))
    | "Span" ->
      let a, l = to_pair (element_contents e) in
      Span (to_attr a, List.map to_inline (Util.to_list l))
    | _ ->
      UnhandledInline e

  and to_block e =
    match element_type e with
    | "BulletList" ->
      let l = Util.to_list (element_contents e) in
      BulletList (List.map to_block_list l)
    | "CodeBlock" ->
      let attr, code = to_pair (element_contents e) in
      CodeBlock (to_attr attr, Util.to_string code)
    | "Header" ->
      let n, a, t = to_triple (element_contents e) in
      Header
        ( Util.to_int n
        , to_attr a
        , to_inline_list t
        )
    | "OrderedList" ->
      let la, l = to_pair (element_contents e) in
      OrderedList
        ( to_list_attributes la
        , List.map to_block_list (Util.to_list l)
        )
    | "Para" ->
      Para (to_inline_list (element_contents e))
    | "Plain" ->
      Plain (to_inline_list (element_contents e))
    | "RawBlock" ->
      let fmt, contents = to_pair (element_contents e) in
      RawBlock (Util.to_string fmt, Util.to_string contents)
    | "Div" ->
      let a, l = to_pair (element_contents e) in
      Div (to_attr a, to_block_list l)
    | "LineBlock" ->
      let l = Util.to_list (element_contents e) in
      LineBlock (List.map to_inline_list l)
    | "BlockQuote" ->
      BlockQuote (to_block_list (element_contents e))
    | "DefinitionList" ->
      let l = Util.to_list (element_contents e) in
      let to_item x =
        let term, defs = to_pair x in
        ( to_inline_list term
        , List.map to_block_list (Util.to_list defs)
        )
      in
      DefinitionList (List.map to_item l)
    | "HorizontalRule" ->
      HorizontalRule
    | "Table" when version_ge [1;22] ->
      let e = element_contents e in
      let a, c, cs, th, tb, tf = to_sextuple e in
      let to_col_width x =
        match element_type x with
        | "ColWidth" -> ColWidth (Util.to_float (element_contents x))
        | "ColWidthDefault" -> ColWidthDefault
        | _ -> assert false
      in
      let to_col_spec x =
        let al, cw = to_pair x in
        to_alignment al, to_col_width cw
      in
      let to_cell x =
        let a, al, rs, cs, l = to_quintuple x in
        Cell
          ( to_attr a
          , to_alignment al
          , RowSpan (Util.to_int rs)
          , ColSpan (Util.to_int cs)
          , to_block_list l
          )
      in
      let to_row x =
        let a, cl = to_pair x in
        Row (to_attr a, List.map to_cell (Util.to_list cl))
      in
      let to_table_head x =
        let a, rl = to_pair x in
        TableHead (to_attr a, List.map to_row (Util.to_list rl))
      in
      let to_table_body x =
        let a, hc, rl, rl' = to_quadruple x in
        TableBody
          ( to_attr a
          , RowHeadColumns (Util.to_int hc)
          , List.map to_row (Util.to_list rl)
          , List.map to_row (Util.to_list rl')
          )
      in
      let to_table_foot x =
        let a, rl = to_pair x in
        TableFoot (to_attr a, List.map to_row (Util.to_list rl))
      in
      Table
        ( to_attr a
        , to_caption c
        , List.map to_col_spec (Util.to_list cs)
        , to_table_head th
        , List.map to_table_body (Util.to_list tb)
        , to_table_foot tf
        )
    | "Figure" ->
      let a, c, l = to_triple (element_contents e) in
      Figure
        ( to_attr a
        , to_caption c
        , to_block_list l
        )
    | _ -> UnhandledBlock e

  and to_inline_list l =
    List.map to_inline (Util.to_list l)

  and to_block_list l =
    List.map to_block (Util.to_list l)

  and to_caption c =
    let o, l = to_pair c in
    Caption
      ( Util.to_option to_inline_list o
      , to_block_list l
      )

  let element t c = `Assoc ["t", `String t; "c", c]

  let element_nc t = `Assoc ["t", `String t]

  let of_attr (id, classes, keyvals) =
    let id = `String id in
    let classes = `List (List.map (fun s -> `String s) classes) in
    let keyvals = `List (List.map (fun (k,v) -> `List [`String k; `String v]) keyvals) in
    `List [id; classes; keyvals]

  let of_list_attr (n, style, delim) =
    let n = `Int n in
    let style = match style with
      | DefaultStyle -> "DefaultStyle"
      | Example -> "Example"
      | Decimal -> "Decimal"
      | LowerRoman -> "LowerRoman"
      | UpperRoman -> "UpperRoman"
      | LowerAlpha -> "LowerAlpha"
      | UpperAlpha -> "UpperAlpha"
    in
    let delim = match delim with
      | DefaultDelim -> "DefaultDelim"
      | Period -> "Period"
      | OneParen -> "OneParen"
      | TwoParensPeriod -> "TwoParensPeriod"
    in
    let style = `Assoc ["t", `String style] in
    let delim = `Assoc ["t", `String delim] in
    `List [n; style; delim]

  let of_target (url, title) =
    `List [`String url; `String title]

  let of_alignment = function
    | AlignLeft -> element_nc "AlignLeft"
    | AlignRight -> element_nc "AlignRight"
    | AlignCenter -> element_nc "AlignCenter"
    | AlignDefault -> element_nc "AlignDefault"

  let of_col_width = function
    | ColWidth w -> element "ColWidth" (`Float w)
    | ColWidthDefault -> element_nc "ColWidthDefault"

  let of_col_spec (a, cw) =
    `List [ of_alignment a; of_col_width cw ]

  let of_math_type = function
    | DisplayMath -> element_nc "DisplayMath"
    | InlineMath -> element_nc "InlineMath"

  let rec of_block = function
    | BulletList l ->
      element "BulletList" (`List (List.map of_blocks l))
    | CodeBlock (a, s) ->
      element "CodeBlock" (`List [of_attr a; `String s])
    | Header (n, a, t) ->
      element "Header" (`List [`Int n; of_attr a; of_inlines t])
    | OrderedList (la, l) ->
      element "OrderedList" (`List [of_list_attr la; `List (List.map of_blocks l)])
    | Para l ->
      element "Para" (of_inlines l)
    | Plain l ->
      element "Plain" (of_inlines l)
    | RawBlock (f, c) ->
      element "RawBlock" (`List [`String f; `String c])
    | Div (a, l) ->
      element "Div" (`List [of_attr a; of_blocks l])
    | LineBlock l ->
      element "LineBlock" (`List (List.map of_inlines l))
    | BlockQuote l ->
      element "BlockQuote" (of_blocks l)
    | DefinitionList l ->
      element "DefinitionList" @@
        `List (l |> List.map (fun (il, bll) ->
          `List [of_inlines il; `List (List.map of_blocks bll)]))
    | HorizontalRule ->
      element_nc "HorizontalRule"
    | Table (a, c, cs, th, tb, tf) ->
      element "Table" @@
        `List
          [ of_attr a
          ; of_caption c
          ; `List (List.map of_col_spec cs)
          ; of_table_head th
          ; `List (List.map of_table_body tb)
          ; of_table_foot tf
          ]
    | Figure (a, c, l) ->
      element "Figure" (`List [of_attr a; of_caption c; of_blocks l])
    | UnhandledBlock b ->
      b

  and of_blocks l =
    `List (List.map of_block l)

  and of_inline = function
    | Code (a, t) ->
      element "Code" (`List [of_attr a; `String t])
    | Emph i ->
      element "Emph" (of_inlines i)
    | Image (a, i, t) ->
      element "Image" (`List [of_attr a; of_inlines i; of_target t])
    | Link (a, i, t) ->
      element "Link" (`List [of_attr a; of_inlines i; of_target t])
    | Quoted (q, i) ->
      let q =
        match q with
        | DoubleQuote -> element_nc "DoubleQuote"
        | SingleQuote -> element_nc "SingleQuote"
      in
      element "Quoted" (`List [q; of_inlines i])
    | RawInline (f, s) ->
      element "RawInline" (`List [`String f; `String s])
    | SmallCaps i ->
      element "SmallCaps" (of_inlines i)
    | Space ->
      element_nc "Space"
    | Str s ->
      element "Str" (`String s)
    | Underline i ->
      element "Underline" (of_inlines i)
    | Strong i ->
      element "Strong" (of_inlines i)
    | Strikeout i ->
      element "Strikeout" (of_inlines i)
    | Superscript i ->
      element "Superscript" (of_inlines i)
    | Subscript i ->
      element "Subscript" (of_inlines i)
    | Cite (ci, i) ->
      element "Cite" (`List [`List (List.map of_citation ci); of_inlines i])
    | SoftBreak ->
      element_nc "SoftBreak"
    | LineBreak ->
      element_nc "LineBreak"
    | Math (m, s) ->
      element "Math" (`List [of_math_type m; `String s])
    | Note l ->
      element "Note" (of_blocks l)
    | Span (a, i) ->
      element "Span" (`List [of_attr a; of_inlines i])
    | UnhandledInline i ->
      i

  and of_inlines l =
    `List (List.map of_inline l)

  and of_caption (Caption (c, l)) =
    `List [Option.fold ~some:of_inlines ~none:`Null c; of_blocks l]

  and of_cell (Cell (at, al, rs, cs, l)) =
    let rs = match rs with RowSpan n -> `Int n in
    let cs = match cs with ColSpan n -> `Int n in
    `List [of_attr at; of_alignment al; rs; cs; of_blocks l]

  and of_row (Row (a, cl)) =
    `List [of_attr a; `List (List.map of_cell cl)]

  and of_table_head (TableHead (a, rl)) =
    `List [of_attr a; `List (List.map of_row rl)]

  and of_table_body (TableBody (a, hc, rl, rl')) =
    let hc = match hc with RowHeadColumns n -> `Int n in
    `List
      [ of_attr a
      ; hc
      ; `List (List.map of_row rl)
      ; `List (List.map of_row rl')
      ]

  and of_table_foot (TableFoot (a, rl)) =
    `List [of_attr a; `List (List.map of_row rl)]

  and of_citation (Citation c) =
    let of_citation_mode = function
      | AuthorInText -> "AuthorInText"
      | SuppressAuthor -> "SuppressAuthor"
      | NormalCitation -> "NormalCitation"
    in
    `Assoc
      [ "citationId", `String c.citation_id
      ; "citationPrefix", of_inlines c.citation_prefix
      ; "citationSuffix", of_inlines c.citation_suffix
      ; "citationMode", element_nc (of_citation_mode c.citation_mode)
      ; "citationNoteNum", `Int c.citation_note_num
      ; "citationHash", `Int c.citation_hash
      ]
end

(** {2 Reading and writing} *)

let of_json json =
  let json = Util.to_assoc json in
  let api_version = List.assoc "pandoc-api-version" json in
  let api_version = List.map Util.to_int (Util.to_list api_version) in
  current_version := api_version;
  let meta = List.assoc "meta" json in
  let blocks = Util.to_list (List.assoc "blocks" json) in
  let blocks = List.map JSON.to_block blocks in
  { blocks; api_version; meta }

let to_json p =
  let blocks = `List (List.map JSON.of_block p.blocks) in
  let api_version = `List (List.map (fun n -> `Int n) p.api_version) in
  let meta = p.meta in
  `Assoc ["blocks", blocks; "pandoc-api-version", api_version; "meta", meta]

(** JSON from markdown file. *)
let json_of_md_file fname =
  let tmp = Filename.temp_file "pandoc" ".json" in
  let cmd = Printf.sprintf "pandoc -f markdown -t json %s -o %s" fname tmp in
  let n = Sys.command cmd in
  assert (n = 0);
  let json = from_file tmp in
  Sys.remove tmp;
  json

let of_md_file fname =
  let json = json_of_md_file fname in
  of_json json

let api_version p = p.api_version

let blocks p = p.blocks

(** {2 Metadata} *)

type meta_value =
  | MetaBool of bool
  | MetaInlines of inline list
  | MetaString of string
  | MetaMap of (string * meta_value) list
  | MetaList of meta_value list
  | MetaBlocks of block list
  | MetaUnhandled of Yojson.Basic.t

let rec of_meta e =
  let contents = JSON.element_contents e in
  match JSON.element_type e with
  | "MetaBool" ->
    MetaBool (Util.to_bool contents)
  | "MetaInlines" ->
    MetaInlines (contents |> Util.to_list |> List.map JSON.to_inline)
  | "MetaString" ->
    MetaString (Util.to_string contents)
  | "MetaBlocks" ->
    MetaBlocks (contents |> Util.to_list |> List.map JSON.to_block)
  | "MetaList" ->
    MetaList (Util.to_list contents |> List.map of_meta)
  | "MetaMap" ->
    let m =
      Util.keys contents
      |> List.map (fun k -> k, of_meta (Util.member k contents))
    in
    MetaMap m
  | _ -> MetaUnhandled e

let meta p =
  let m = Util.to_assoc p.meta in
  List.map (fun (k, v) -> k, of_meta v) m

let meta_bool p k =
  match List.assoc k (meta p) with
  | MetaBool b -> b
  | MetaString "yes"
  | MetaInlines [Str "yes"] -> true
  | MetaString "no"
  | MetaInlines [Str "no"] -> false
  | _ -> raise Not_found

let meta_string p k =
  match List.assoc k (meta p) with
  | MetaInlines l ->
    List.map (function Str s -> s | Space -> " " | _ -> raise Not_found) l |> String.concat ""
  | MetaString s -> s
  | _ -> raise Not_found

(** {2 Transforming} *)

(** Change the list of blocks. *)
let replace_blocks f p =
  { p with blocks = f p.blocks }

let map ?(block=(fun _ -> None)) ?(inline=(fun _ -> None)) p =
  let rec map_block b =
    match block b with
    | Some bb -> bb
    | None ->
      match b with
      | CodeBlock _ | RawBlock _ | HorizontalRule | UnhandledBlock _ ->
        [b]
      | Para ii -> [Para (map_inlines ii)]
      | Plain ii -> [Plain (map_inlines ii)]
      | Div (la, l) -> [Div (la, map_blocks l)]
      | BulletList l -> [BulletList (List.map map_blocks l)]
      | Header (n, a, t) -> [Header (n, a, map_inlines t)]
      | OrderedList (la, l) -> [OrderedList (la, List.map map_blocks l)]
      | LineBlock l -> [LineBlock (List.map map_inlines l)]
      | BlockQuote l -> [BlockQuote (map_blocks l)]
      | DefinitionList l ->
        [DefinitionList (l |> List.map (fun (term, defs) ->
          (map_inlines term, List.map map_blocks defs)))]
      | Table (a, c, cs, th, tb, tf) ->
        let map_cell (Cell (a, al, rs, cs, l)) =
          Cell (a, al, rs, cs, map_blocks l) in
        let map_row (Row (a, cells)) =
          Row (a, List.map map_cell cells) in
        let map_th (TableHead (a, rows)) =
          TableHead (a, List.map map_row rows) in
        let map_tb (TableBody (a, c, rows, rows')) =
          TableBody (a, c, List.map map_row rows, List.map map_row rows') in
        let map_tf (TableFoot (a, rows)) =
          TableFoot (a, List.map map_row rows) in
        [Table (a, c, cs, map_th th, List.map map_tb tb, map_tf tf)]
      | Figure (a, c, l) -> [Figure (a, c, map_blocks l)]

  and map_inline i =
    match inline i with
    | Some ii -> ii
    | None ->
      match i with
      | Code _ | Space | Str _ | RawInline _ | Math _
      | SoftBreak | LineBreak | UnhandledInline _ ->
        [i]
      | Emph i -> [Emph (map_inlines i)]
      | Image (a, i, t) -> [Image (a, map_inlines i, t)]
      | Link (a, i, t) -> [Link (a, map_inlines i, t)]
      | Quoted (q, i) -> [Quoted (q, map_inlines i)]
      | SmallCaps i -> [SmallCaps (map_inlines i)]
      | Underline i -> [Underline (map_inlines i)]
      | Strong i -> [Strong (map_inlines i)]
      | Strikeout i -> [Strikeout (map_inlines i)]
      | Superscript i -> [Superscript (map_inlines i)]
      | Subscript i -> [Subscript (map_inlines i)]
      | Cite (c, i) -> [Cite (c, map_inlines i)]
      | Note l -> [Note (map_blocks l)]
      | Span (a, i) -> [Span (a, map_inlines i)]
  and map_blocks bb = List.flatten (List.map map_block bb)
  and map_inlines ii = List.flatten (List.map map_inline ii) in
  replace_blocks map_blocks p

let map_inlines f p =
  let block = function
    | Para ii -> Para (f ii)
    | b -> b
  in
  replace_blocks (List.map block) p

let map_blocks f p = map ~block:f p

(** Map a function to every top-level block. *)
let map_top_blocks f p =
  replace_blocks (fun blocks -> List.flatten (List.map f blocks)) p
