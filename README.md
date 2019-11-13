# Introduction

This is a utility we wrote in order to both reaquaint ourselves with Ocaml and
allow making bots with a reasonably large network(assuming that it can be
representated as a DAG/DFA).

This requires [Dune](https://github.org/Ocaml/dune),
[Angstrom](https://github.com/inhabitedtype/angstrom/),
[Zarith](https://github.com/ocaml/zarith), and most likely
[Opam](https://opam.ocaml.org), if only because it makes obtaining these
dependencies significantly easier.

Once you have those installed and opam's environment setup, you should be able to run 
`dune build main.exe` (the .exe is present on all platforms).

If that completes successfully, then you can follow up with
`_build/default/main.exe example.bot total` to get some output.

# Writing your own bots rules

The input syntax is, at least to our jaded eyes, reasonably simple.

The toplevel statements are 'assignment', which goes `<symbol>=(other
elements);` and declares a new rule.

For example

```
<hello> = { "hello" {"world"|"person"}} "\.";
```

Creates a single rule "hello", which can evaluate to "hello world." and "hello
person." Literal strings are enclosed with double quotes `"`. When the string
starts with a backslash(`\\`), it does not have a space inserted before it.
Whitespace inside the declaration outside strings or symbol names is ignored.

That leaves mostly how rules interact.

1. The rule must be declared before use.
2. The rule cannot appear in its own body.
3. Where the first two conditions are satisfied, the rule will act as if it had
   been pasted in.
4. Rules may be redeclared, and the most recent declaration takes precedence for
   following usage. In this case, the rule may appear in its body.
   ```
   <a> = "a";
   <b> = "b" <a>;
   <a> = <a> <b>;
   ```
   `a` should be equal to `"a b a"`
5. Rules should not be called `<#>` unless they're meant to act as comments or
   not show up in the listing of tags.

In addition to the rules, the files may include comments such that work similarly to shell comments
```
#These are some colors :3
<color> = { "red" | "blue" | "green" };
#this is something else
<something-else>= "";
```

## Context Carrying

Context carrying(for lack of a better term), is a way to carry some terms forwards
through the rules. For example

```
<a> = (girl's-name).{ "Melissa" | "Jessica" | "Nona" } 
      "enjoyed some" {"Vanilla"|"Chocolate"} "icecream." 
      (girl's-name) "likes that flavor";
```

Allows the second usage of `girl's-name` to match the first without a great deal more states.
