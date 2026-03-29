open Types


let rec string_term = function 
  | VAR(x) -> x 
  | ABS(x, t) -> Printf.sprintf "λ%s.%s" x (string_term t)
  (* | APP(VAR(x), t2) -> Printf.sprintf "%s %s" x (string_term t2)
  | APP(t1, VAR(y)) -> Printf.sprintf "(%s) %s" (string_term t1) y  *)
  | APP(t1, t2) -> Printf.sprintf "(%s) (%s)" (string_term t1) (string_term t2)

let print_bind = function 
  | LET(x, t) -> Printf.printf "let %s = %s in" x (string_term t) 
  | LETREC(x, t) -> Printf.printf "let rec %s = %s in" x (string_term t) 

let print_bindings = List.iter (fun b -> print_bind b; print_newline ())

let print_term t = print_string (string_term t)

let print_program prog = 
  print_bindings prog.bindings;
  print_term prog.term;
  print_newline ()
