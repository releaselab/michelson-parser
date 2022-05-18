(lang dune 2.8)

(name michelson-parser)

(generate_opam_files true)

(source
 (uri "git+https://github.com/joaosreis/michelson-parser.git"))

(bug_reports "https://github.com/joaosreis/michelson-parser/issues")

(homepage "https://github.com/joaosreis/michelson-parser")

(license MIT)

(authors "Jo\195\163o Santos Reis <joaosreis@outlook.pt>")

(maintainers "Jo\195\163o Santos Reis <joaosreis@outlook.pt>")

(package
 (name michelson-parser)
 (version 0.1)
 (synopsis "An OCaml library to parse Michelson smart contracts")
 (depends
  (dune :build)
  (core
   (>= 0.15))
  (core_unix
   (and
    :with-test
    (>= 0.15)))
  michelson-adt
  (tezos-micheline
   (>= 7.0))
  ppx_deriving
  (pp_loc
   (>= 2.0))
  (bisect_ppx
   (and
    :with-test
    (>= 2.5.0)))
  (alcotest :with-test)))
