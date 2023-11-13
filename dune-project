(lang dune 2.0)
(version 0.3.0)
(name pandoc)
(source (github smimram/ocaml-pandoc))
(license LGPL-2.1-or-later)
(authors "Samuel Mimram <smimram@gmail.com>")
(maintainers "Samuel Mimram <smimram@gmail.com>")

(generate_opam_files true)

(package
 (name pandoc)
 (synopsis "Library to write pandoc filters")
 (description "This library helps writing filters for pandoc, which is a tool to convert between textual formats notably, markdown and LaTeX.")
 (depends
  (ocaml (>= 4.08.0))
  (yojson (>= 1.4.0))
 )
)

(package
 (name pandoc-abbreviations)
 (synopsis "Pandoc filter to add non-breaking spaces after abbreviations")
 (description "This pandoc filter adds non-breaking spaces after the abbreviations listed in the `abbreviations` file.")
 (depends
  (ocaml (>= 4.04.0))
  (pandoc (>= 0.1.0))
 )
)

(package
 (name pandoc-crossref)
 (synopsis "Pandoc filter to have LaTeX cross-references")
 (description "This pandoc filter replaces #chap: and #sec: references to cref invokations.")
 (depends
  (pandoc (>= 0.1.0))
 )
)

(package
 (name pandoc-include)
 (synopsis "Pandoc filter to include other files")
 (description "This pandoc filter allows the inclusion of other markdown files into markdown.")
 (depends
  (pandoc (>= 0.1.0))
 )
)

(package
 (name pandoc-inspect)
 (synopsis "Pandoc filter to inspect pandoc's JSON")
 (description "This pandoc filter prints pandoc's internal JSON on the standard error.")
 (depends
  (pandoc (>= 0.1.0))
 )
)

(package
 (name pandoc-pdf2png)
 (synopsis "Pandoc filter to change extension of pdf images to png")
 (description "This pandoc filter changes the extension of images from pdf to png.")
 (depends
  (pandoc (>= 0.1.0))
 )
)

(package
 (name pandoc-comment)
 (synopsis "Pandoc filter to remove comments")
 (description "This pandoc filter removes LaTeX-style comments (line starting with %).")
 (depends
  (pandoc (>= 0.1.0))
 )
)

(package
 (name pandoc-replace)
 (synopsis "Pandoc filter to replace words")
 (description "This pandoc filter replaces words by others (useful to ensure that the case is respected for instance).")
 (depends
  (pandoc (>= 0.1.0))
 )
)
