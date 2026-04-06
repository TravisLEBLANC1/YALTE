
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


type ctxt_type = 
  | LAPP of  (* hole * *) term 
  | RAPP of term (* * hole *)
  | CABS of ident (* * hole *)

(* context are reprensented upside down, 
the first element is the element just above the hole
the empty list is just a hole *)
type context = ctxt_type list 

type input_type = Church | Scott | No_Input

let var_equal = String.equal

let rec find_abs x ctxt res = match ctxt with 
  | [] -> None
  | CABS(y)::ctxt when String.equal x y -> Some(res @ [CABS(y)], ctxt)
  | c::ctxt -> find_abs x ctxt (res @ [c])

(* unfold the context until we find the ABS(x), 
  if we have C[λx.D[x]], then it returns (λx.D, C)*)
let find_abs x ctxt = find_abs x ctxt []

(* reconstruct the term by filling the hole with filler*)
let rec fill_hole ctxt filler = match ctxt with 
  | [] -> filler 
  | CABS(y)::ctxt -> fill_hole ctxt (ABS(y, filler))
  | LAPP(t)::ctxt -> fill_hole ctxt (APP(filler, t))
  | RAPP(t)::ctxt -> fill_hole ctxt (APP(t, filler))

(* return the set of free variable of t*)
let rec fv = function 
  | VAR(x) -> SSet.singleton x 
  | APP(t1, t2) -> SSet.union (fv t1) (fv t2)
  | ABS(x, t) -> SSet.diff (fv t) (SSet.singleton x)

let rec substitute x v = function 
  | VAR(y) -> if String.equal y x then v else VAR(y)
  | ABS(y, t) -> if String.equal y x then ABS(y, t) else ABS(y, substitute x v t)
  | APP(t1, t2) -> APP(substitute x v t1, substitute x v t2) 

(* TODO: manage aplha renaming ? *)
let substitute_in_letbind x v = function 
  | LET(y, t) -> LET(y, substitute x v t)
  | LETREC(y, t) -> LETREC(y, substitute x v t)


let rec int_to_scott n = match n with 
  | 0 -> ABS("z", ABS("s", VAR("z")))
  | n -> ABS("z", ABS("s", APP(VAR("s"), int_to_scott (n-1))))

let int_to_church n = 
  let nf = ref (VAR("x")) in 
  for _ = 0 to n-1 do
    nf := APP(VAR("f"), !nf)
  done;
  ABS("f", ABS("x", !nf))

let rec prog_to_term_aux bindings term app_term : term = 
  match bindings with 
  | [] -> 
    if (Option.is_some app_term) then 
      APP(term, Option.get app_term)
    else
      term
  | LET(x, v) :: bindings -> 
    let newbindings = List.map (substitute_in_letbind x v) bindings in 
    let newterm = substitute x v term in 
    prog_to_term_aux newbindings newterm app_term
  | LETREC(x, v) :: bindings -> 
    let newbindings = List.map (substitute_in_letbind x v) bindings in 
    let newterm = substitute x v term in 
    prog_to_term_aux newbindings newterm app_term


(* substitute all bindings to create a single list of term*)
let prog_to_term (prog : prog) (input_type : input_type) (n : int) = 
  let app_term = match input_type with 
        | No_Input -> None
        | Church -> Some(int_to_church n)
        | Scott -> Some(int_to_scott n)
  in
   
  List.map (fun t -> prog_to_term_aux prog.bindings t app_term) prog.term 
