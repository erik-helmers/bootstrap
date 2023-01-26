(lang dune 3.6)

(name bs)

(using menhir 2.1)

(generate_opam_files true)

(source
 (github erik-helmers/bs))

(authors "Erik Helmers")

(maintainers "Erik Helmers")

(license NONE)

(package
 (name bs)
 (synopsis "BS : A type project")
 (description "")
 (depends ocaml dune menhir ppx_import)
 (tags
  (topics "research")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
