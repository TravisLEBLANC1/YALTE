open Term
open Print


type closure = Clos of term * env
and env =  EnvNil | EnvCons of (ident * closure) * env

type stack = closure list
type config = Conf of term * env * stack

(*** printing functions ***)

let rec sprint_closure (Clos(t, e))=
  Printf.sprintf "(%s, %s)" (sprint_term t) (sprint_env e)

and sprint_env = function 
  | EnvNil -> "[]"
  | EnvCons((x, c), e) -> Printf.sprintf "[%s <- %s]::%s" x (sprint_closure c) (sprint_env e)

and sprint_stack = function 
  | [] -> "[]"
  | c::s -> Printf.sprintf "%s::%s" (sprint_closure c) (sprint_stack s)

let sprint_config (Conf(t, e, s)) = 
  [| sprint_term t ; sprint_env e ; sprint_stack s |]


let print_kam_run out (run : config list) = 
  let header : string array = [| "Term"; "Env"; "Stack" |] in
  let configs = Array.of_list (List.map sprint_config run) in
  Print.print_run out header configs

(*** transitions functions ***)

(* return the projection of the env to the free variables of t*)
let projection (t : term) (e : env) : env = 
  let free = fv t in 
  let rec proj_aux = function
    | EnvNil -> EnvNil 
    | EnvCons((x, c), e) -> 
      if SSet.mem x free then 
        EnvCons((x, c), proj_aux e)
      else
        proj_aux e 
  in 
  proj_aux e

(* return the next config of the KAM *)
let trans (Conf(t, e, s) : config) : config = 
  match t with  
  | APP(t1, t2) ->
    Conf(t1, projection t1 e, Clos(t2, projection t2 e) :: s)
  | ABS(x, t1) -> 
    begin
      match s with 
      | [] -> Conf(t, e, s) (* reached final state *)
      | c::s -> Conf(t1, EnvCons((x, c), e), s)
    end
  | VAR(x) -> 
    begin
      match e with 
      | EnvCons((y, Clos(t, e')), EnvNil) -> 
        if not @@ String.equal x y then 
          raise (Failure("wrong env " ^ x ^ " != " ^ y))
        else
          Conf(t, e', s)
      | _ -> raise (Failure("wrong env: is not a singleton on " ^ x))
    end

let is_final (Conf(t, _, s) : config) =
  match t,s with 
  | ABS(_,_), [] -> true 
  | _ -> false

let rec kam_loop (c : config) : config list = 
  if is_final c then 
    [c]
  else
    c :: kam_loop (trans c)

let kam (t : term) : config list = kam_loop (Conf(t, EnvNil, []))

