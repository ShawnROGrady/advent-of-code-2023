(lang dune 3.4)

(name advent_of_code_2023)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (allow_empty)
 (name advent_of_code_2023)
 (synopsis "Advent of code 2023")
 (description "Solutions to advent of code 2023")
 (depends ocaml dune (fmt (>= 0.9.0)) (zarith (>= 1.12))))
