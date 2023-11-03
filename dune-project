(lang dune 3.10)

(name opoker)

(generate_opam_files true)

(source
 (github flocks/opoker))

(authors "Florent Teissier")

(maintainers "Florent Teissier")

(license LICENSE)

(documentation https://github.com/flocks/opoker)

(package
 (name opoker)
 (synopsis "Computer poker hands values")
 (description "Get NHLE poker hands values")
 (depends ocaml dune)
 (tags
  (topics "poker" "nhle" "hands" "values")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
