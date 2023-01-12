# CS3310: Data Structures and Functional Programming

**Fall 2022 Edition.**

TEXTBOOK: [OCaml Programming: Correct + Efficient + Beautiful](https://cs3110.github.io/textbook/index.html)


## After installing Ocaml

First, initial opam, create a new ocaml switch, and install necessary packages, 
following the installation tutorial.

```bash
opam init --bare -a -y
opam switch create cs3110-2022fa ocaml-base-compiler.4.14.0
eval $(opam env)
opam install -y utop odoc ounit2 qcheck bisect_ppx menhir ocaml-lsp-server ocamlformat ocamlformat-rpc
```

Then install some packages we would like to use in this course.

```bash
opam install -y zarith
```

Finally, we can launch `utop` now!


## Learning Ocaml

```mermaid
graph LR
A(Ocaml language)
B1(Syntax)
B2(Sematics)
C21(Dynamic sematics)
C22(Static sematics)
D21(Evaluation rules)
D22(Type-checking rules)
D23(Patern-matching rules)
D31(Exhaustiveness of patterns)
D32(Unused branches)
B3(Idioms)
B4(Libraries)
B5(Tools)
A --> B1 & B2 & B3 & B4 & B5
B2 --> C21 & C22
C21 --> D21
C22 --> D22 & D23
D23 --> D31 & D32
```