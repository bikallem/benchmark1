(lang dune 3.0)

(generate_opam_files true)

(using mdx 0.2)

(source
 (github bikallem/benchmark1))

(authors "Bikal Gurung")

(maintainers "Bikal Gurung")

(license MPL2.0)

(package
 (name benchmark1))

(package
 (name cohttp_parser)
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

(package
 (name angstrom_parser))
