
module SSet = Set.Make(String)

type ident = string

type prog = {
  bindings : letbinds;
  term : term 
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