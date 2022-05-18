# Michelson parser

![main workflow](https://github.com/joaosreis/michelson-parser/actions/workflows/main.yml/badge.svg)

An OCaml library to parse Michelson smart contracts into an
[ADT](https://github.com/joaosreis/michelson-adt).

## Install instructions

### Using dune

```bash
git clone https://github.com/joaosreis/michelson-parser.git
cd michelson-parser
dune build @install
dune install
```

### Using opam

```bash
opam install https://github.com/joaosreis/michelson-parser.git
```

---

Developed under the [FRESCO](https://release.di.ubi.pt/projects/fresco.html)
project (Formal Verification of Smart Contracts), generously funded by [Tezos
Foundation](https://tezos.foundation).
