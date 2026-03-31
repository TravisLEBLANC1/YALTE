
module SSet = Set.Make(String)

type ident = string

type prog = {
  bindings : letbinds;
  term : term list; 
}
and letbinds = letbind list
and letbind = 
  | LET of ident * term 
  | LETREC of ident * term
and term = 
  | VAR of ident 
  | ABS of ident * term 
  | APP of term * term



let rec fv = function 
  | VAR(x) -> SSet.singleton x 
  | APP(t1, t2) -> SSet.union (fv t1) (fv t2)
  | ABS(x, t) -> SSet.diff (fv t) (SSet.singleton x)

let rec substitute x v = function 
  | VAR(y) -> if String.equal y x then v else VAR(y)
  | ABS(y, t) -> if String.equal y x then ABS(y, t) else ABS(y, substitute x v t)
  | APP(t1, t2) -> APP(substitute x v t1, substitute x v t2) 

(* TODO: manage aplha renaming *)
let substitute_in_letbind x v = function 
  | LET(y, t) -> LET(y, substitute x v t)
  | LETREC(y, t) -> LETREC(y, substitute x v t)


let rec prog_to_term_aux bindings term : term = 
  match bindings with 
  | [] -> term 
  | LET(x, v) :: bindings -> 
    let newbindings = List.map (substitute_in_letbind x v) bindings in 
    let newterm = substitute x v term in 
    prog_to_term_aux newbindings newterm
  | LETREC(x, v) :: bindings -> 
    let newbindings = List.map (substitute_in_letbind x v) bindings in 
    let newterm = substitute x v term in 
    prog_to_term_aux newbindings newterm


(* substitute all bindings to create a single list of term*)
let prog_to_term (prog : prog) = List.map (prog_to_term_aux prog.bindings) prog.term 
