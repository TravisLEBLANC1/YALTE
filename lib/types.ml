
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
