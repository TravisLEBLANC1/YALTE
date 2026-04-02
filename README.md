# YALTE
Yet Another Lambda-Term Evaluator

## Build

build with `dune build`

There is a dependency with the package [Wcwidth](https://opam.ocaml.org/packages/wcwidth/) for printing purposes


## Usage


`./bin/main.exe [-iam/-kam] [-v] prog.ml`
- `-iam` activate the IAM
- `-kam` activate the KAM
- `-v` will print the entire run
- `-n n` the number of bullet inital of the iam
- `-scott n` gives n as input to the final term, in scott's encoding 
- `-church n` gives n as input to the final term, in church's encoding 

typically the command I use to get the length of the computation is :
`./bin/main.exe -scott 10 -iam prog.ml`

and to check the run I use :
`./bin/main.exe -v -scott 10 -iam prog.ml > output.txt` 

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

see a complete example at [[input_example.ml]]