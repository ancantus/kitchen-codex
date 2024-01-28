(lang dune 3.12)

(name kitchen_codex)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Ryan Estep")

(maintainers "Ryan Estep")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name kitchen_codex)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
