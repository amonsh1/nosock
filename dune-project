(lang dune 3.4)

(name nosock)

(generate_opam_files true)

(source
 (github amonsh1/nosock))

(authors "amonsh1")

(maintainers "amonsh1")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name nosock)
 (synopsis "Ocaml websocket client implementation")
 (depends ocaml dune)
 (tags
  (websocket client ocaml)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
