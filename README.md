# YALTE
Yet Another Lambda-Term Evaluator

## Build

build with `dune build`

There is a dependency with the package [Wcwidth](https://opam.ocaml.org/packages/wcwidth/) for printing purposes


## Usage


`./bin/main.exe [-iam/-kam] [-v] prog.ml`
- -iam activate the IAM
- -kam activate the KAM
- -v will print the entire run

## Syntax

The program syntax is as follow:

```ocaml
let x1 = t1 in 
let x2 = t2 in 
...
(*some coms*)
let x3 = t3 in 

t4 | t5 | t6
```

this will execute t4 then t5 then t6 separately, and print all 3 results

the terms are lambda terms of the following grammar:
```
t := \x.t | λx.t | t t | x
```

/!\ careful we accept indentifier of more than one letter so we need spaces for application that is:

```ocaml
λx.xx  (*is interpreted as the term λy.xx, where xx is a free variable*)
λx.x x (*is interpreted as the term λy.y y with no free variables*)
```
