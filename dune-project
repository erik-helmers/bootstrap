(lang dune 3.6)

(name scratch)

(generate_opam_files true)

(source
 (github erik-helmers/bootstrap))

(authors "Erik Helmers" "Pierre-Evariste Dagand")

(license MIT)

(documentation https://github.com/erik-helmers/bootstrap)


(package
 (name scratch)
 (synopsis "Scratch")
 (description "Implémentation d'une proposition indécente")
 (depends ocaml dune alcotest pprint mdx)
 (tags
  (topics "typing")))

(using mdx 0.3)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
