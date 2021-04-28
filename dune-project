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
