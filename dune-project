(lang dune 3.0)

(name benchmark1)

(generate_opam_files true)

(source
 (github bikallem/benchmark1))

(authors "Bikal Gurung")

(maintainers "Bikal Gurung")

(license MPL2.0)

(package
 (name benchmark1)
 (synopsis "Benchmark experiments")
 (description "Benchmark experiments")
 (depends
  (ocaml
   (<= 4.12+domains))
  dune
  core_bench
  core
  bechamel
  bechamel-notty))
