(lang dune 2.0)
(version 0.1.0)
(name pandoc)
(source (github smimram/ocaml-pandoc))
(license GPL-2.0)
(authors "Samuel Mimram <smimram@gmail.com>")
(maintainers "Samuel Mimram <smimram@gmail.com>")

(generate_opam_files true)

(package
 (name pandoc)
 (synopsis "Library to write pandoc filters")
 (description "This library helps writing filters for pandoc, which is a tool to convert between textual formats notably, markdown and LaTeX")
 (depends
  (dune (>= 2.0))
  (yojson (>= 1.4.0))
 )
)

(package
 (name pandoc-abbreviations)
 (synopsis "Pandoc filter to add non-breaking spaces after abbreviations")
 (description "This pandoc filter adds non-breaking spaces after the abbreviations listed in the `abbreviations` file")
 (depends
  (dune (>= 2.0))
  (pandoc (>= 0.1.0))
 )
)

(package
 (name pandoc-crossref)
 (synopsis "Pandoc filter to have LaTeX cross-references")
 (description "This pandoc filter replaces #chap: and #sec: references to cref invokations")
 (depends
  (dune (>= 2.0))
  (pandoc (>= 0.1.0))
 )
)

(package
 (name pandoc-include)
 (synopsis "Pandoc filter to include other files")
 (description "This pandoc filter allows the inclusion of other markdown files into markdown")
 (depends
  (dune (>= 2.0))
  (pandoc (>= 0.1.0))
 )
)

(package
 (name pandoc-inspect)
 (synopsis "Pandoc filter to inspect pandoc's JSON")
 (description "This pandoc filter prints pandoc's internal JSON on the standard error")
 (depends
  (dune (>= 2.0))
  (pandoc (>= 0.1.0))
 )
)
