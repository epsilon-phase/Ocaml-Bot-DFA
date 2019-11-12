# Introduction

This is a utility we wrote in order to both reaquaint ourselves with Ocaml and
allow making bots with a very large output parameter(assuming that it can be
representated as a DAG/DFA).

This requires [Dune](https://github.org/Ocaml/dune),
[Angstrom](https://github.com/inhabitedtype/angstrom/), and most likely
[Opam](https://opam.ocaml.org).

Once you have those installed and opam's environment setup, you should be able to run 
`dune build main.exe` (the .exe is present on all platforms).

If that completes successfully, then you can follow up with
`_build/default/main.exe example.bot total` to get some output.

# Writing your own bots

(Pardon the TODO)
