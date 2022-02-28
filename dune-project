(lang dune 3.0)

(generate_opam_files true)

(source
 (github bikallem/benchmark1))

(authors "Bikal Gurung")

(maintainers "Bikal Gurung")

(license MPL2.0)

(package
 (name parsers)
 (synopsis "Benchmark experiments")
 (description "Benchmark experiments")
 (depends
  (ocaml-variants
   (<= 4.12.0+domains))
  dune
  core_bench
  angstrom
  http
  cstruct
  eio
  core))
