The OCaml pandoc library
========================

This library is meant to help creating filters for [pandoc](https://pandoc.org/)
which is tool to convert between textual formats, notably markdown and LaTeX. It
is vaguely inspired of the corresponding [Haskell
library](http://hackage.haskell.org/package/pandoc-types).

Building filters
----------------

Basically, a pandoc filter will

1. read JSON data from the standard input with `Yojson.Basic.from_channel`,
2. convert it to the library's standard representation with `Pandoc.of_json`,
3. transform it using the `Pandoc.map` function which recursively maps functions
  on various elements,
4. convert the result to JSON with `Pandoc.of_json`,
5. print the result on the standard output using `Yojson.Basic.to_string`.

Once you have build your filter (say, `myfilter`) you can use it to transform
pandoc documents while they are being processed by using the `--filter`
commandline argument. For instance:

```bash
pandoc --filter=myfilter file.md -o file.pdf
```

Examples
--------

Some examples are provided in the [examples](examples/) directory for you
convenience (or because I needed those).

- `pandoc-abbreviations`: adds non-breakable spaces after abbreviations (listed
  in `abbreviations` file). Pandoc already does this natively, but it does not
  play well with other plugins.
- `pandoc-crossref`: handles cross-references in LaTeX. It replaces references
  of the form `#chap:bla` and `#sec:bli` to `\cref{chap:bla}` and
  `\cref{sec:bli}` respectively.
- `pandoc-include`: includes other documents. It replaces

  ```
  !include "file"
  ```
  
  by the contents of the file `file` and
  
  ~~~
  ```{.ocaml include="test.ml" from=2 to=5}
  ```
  ~~~
  
  by the contents of the file `test.ml` between lines 2 and 5 (negative values
  for `to` are counted from the end of the file). Useful for including small
  code snippets.
- `pandoc-inspect`: acts as the identity plugin, but prints the JSON output
  given by pandoc on the standard error. Useful for debugging and adding missing
  features to the library.
- `pandoc-pdf2png`: changes the extension of images from `.pdf` to
  `.png`. Useful if you want to compile both to pdf and html.
- `pandoc-replace`: replaces words by others. Useful for fixing capitalization
  for instance.

Bugs and features requests
--------------------------

The code is simple and stable enough for me but some features are missing. Feel
free to fill [bug reports](https://github.com/smimram/ocaml-pandoc/issues) or
submit [pull requests](https://github.com/smimram/ocaml-pandoc/pulls).
