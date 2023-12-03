(lang dune 3.5)

(name aoc2023)

(generate_opam_files true)

(source
 (github tanelso2/advent-of-code-2023))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name aoc2023)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune base stdio ppx_deriving)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
