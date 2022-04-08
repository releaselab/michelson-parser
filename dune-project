(lang dune 2.8)

(name michelson-parser)

(generate_opam_files true)

(source
 (uri "git+https://github.com/releaselab/michelson-parser.git"))

(bug_reports "https://github.com/releaselab/michelson-parser/issues")

(homepage "https://github.com/releaselab/michelson-parser")

(license MIT)

(authors "Jo\195\163o Santos Reis <joao.reis@ubi.pt>")

(maintainers "Jo\195\163o Santos Reis <joao.reis@ubi.pt>")

(package
 (name michelson-parser)
 (version 0.1)
 (synopsis "An OCaml library to parse Michelson smart contracts")
 (depends
  (dune :build)
  core
  michelson-adt
  (tezos-micheline
   (>= 7.0))
  ppx_deriving
  (bisect_ppx
   (and
    :with-test
    (>= 2.5.0)))
  (alcotest :with-test)))
