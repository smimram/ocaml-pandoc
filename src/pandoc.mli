(** Library to write pandoc filters. *)

(** {2 Types for pandoc} *)

(** Attributes: identifier, classes, key/values. *)
type attr = string * string list * (string * string) list

(** Target of a link: url and title. *)
type target = string * string

type list_number_style = DefaultStyle | Example | Decimal | LowerRoman | UpperRoman | LowerAlpha | UpperAlpha

type list_number_delim = DefaultDelim | Period | OneParen | TwoParensPeriod

type list_attributes = int * list_number_style * list_number_delim

type quote_type = DoubleQuote | SingleQuote

(** Format for raw blocks. *)
type format = string

(** Type of math element (display or inline). *)
type math_type = DisplayMath | InlineMath

type citation_mode = AuthorInText | SuppressAuthor | NormalCitation

(** Alignment of a table column. *)
type alignment = AlignLeft | AlignRight | AlignCenter | AlignDefault

(** The width of a table column, as a percentage of the text width. *)
type col_width = ColWidth of float | ColWidthDefault

(** The specification for a single table column. *)
type col_spec = alignment * col_width

(** The number of rows occupied by a cell; the height of a cell. *)
type row_span = RowSpan of int

(** The number of columns occupied by a cell; the width of a cell. *)
type col_span = ColSpan of int

(** The number of columns taken up by the row head of each row of a
  'TableBody'. The row body takes up the remaining columns. *)
type row_head_columns = RowHeadColumns of int

(** Inline elements. *)
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

(** Block elements. *)
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

(** The caption of a table or figure, with optional short caption. *)
and caption = Caption of short_caption option * block list

(** A short caption, for use in, for instance, lists of figures. *)
and short_caption = inline list

(** A table cell. *)
and cell = Cell of attr * alignment * row_span * col_span * block list

(** A table row. *)
and row = Row of attr * cell list

(** The head of a table. *)
and table_head = TableHead of attr * row list

(** A body of a table, with an intermediate head, intermediate body,
  and the specified number of row header columns in the intermediate body. *)
and table_body = TableBody of attr * row_head_columns * row list * row list

(** The foot of a table. *)
and table_foot = TableFoot of attr * row list

(** JSON representation of a pandoc file. *)
type t

(** {2 Reading and writing} *)

(** Internal representation from JSON representation. *)
val of_json : Yojson.Basic.t -> t

(** JSON representation from internal representation. *)
val to_json : t -> Yojson.Basic.t

(** Construct representation of a markdown file. *)
val of_md_file : string -> t

(** API version. *)
val api_version : t -> int list

(** Blocks. *)
val blocks : t -> block list

(** {2 Metadata} *)

type meta_value =
  | MetaBool of bool
  | MetaInlines of inline list
  | MetaString of string
  | MetaMap of (string * meta_value) list
  | MetaList of meta_value list
  | MetaBlocks of block list
  | MetaUnhandled of Yojson.Basic.t

(** Document metadata. *)
val meta : t -> (string * meta_value) list

(** Value of a boolean metadata. *)
val meta_bool : t -> string -> bool

(** Value of a string metadata. *)
val meta_string : t -> string -> string

(** Add a metadata value *) 
val set_meta :  t -> string -> meta_value -> t

(** {2 Mapping functions} *)

(** General mapping function which maps a function on blocks and a function on
    inlines. If the functions return [None] the mapping is further recursed
    into. *)
val map : ?block:(block -> block list option) -> ?inline:(inline -> inline list option) -> t -> t

(** Map a function to every list of inlines. *)
val map_inlines : (inline list -> inline list) -> t -> t

(** Map a function to every block. If the function returns [None] then the block
    is further recursed into. *)
val map_blocks : (block -> block list option) -> t -> t

(** Map a function to every block at toplevel. *)
val map_top_blocks : (block -> block list) -> t -> t
