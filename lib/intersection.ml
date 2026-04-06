(* 
the goal of this file is to find the intersections types of term
to do this we will first execute the head reduction as usual
then proceed backward by typing the result with $*$
*)

open Term 
open Print

type inter_ty =  Multi of (inter_ty list) | Star | Arrow of inter_ty * inter_ty | Unknown


let rec is_linear_type = function 
  | Multi(_) -> false 
  | Star -> true 
  | Arrow(ty1, ty2) -> is_multi_type ty1 && is_linear_type ty2  
  | Unknown -> Printf.printf "oh wel...\n"; false
and is_multi_type = function 
  | Multi(_) -> true 
  | Unknown -> Printf.printf "oh welll...\n"; false
  | _ -> false

type typed_term = 
  | TVAR of ident * inter_ty 
  | TAPP of typed_term * typed_term * inter_ty 
  | TABS of ident * typed_term * inter_ty 
  | TMulti of typed_term list * inter_ty list


let type_of = function 
  | TVAR(_, ty) -> ty 
  | TAPP(_, _, ty) -> ty
  | TABS(_, _, ty) -> ty
  | TMulti(_, lty) -> Multi(lty)

let rec ty_toplevel (t : term) (ty : inter_ty) = 
  match t with 
  | VAR(x) -> TVAR(x,ty)
  | APP(t1, t2) -> TAPP(ty_toplevel t1 Unknown, ty_toplevel t2 Unknown, ty)
  | ABS(x, t) -> TABS(x, ty_toplevel t Unknown, ty)

let rec remove_types = function
  | TVAR(x,_) -> VAR(x)
  | TAPP(t1, t2, _) -> APP(remove_types t1, remove_types t2)
  | TABS(x, t, _) -> ABS(x, remove_types t)
  | TMulti([],_) -> VAR("E")
  | TMulti(x::_,_) -> remove_types x

(* we should collect bellow a term if it is not typed with an axiom rule*)
let should_collect_bellow = function 
  | TVAR(_, Multi([])) -> false 
  | TAPP(_, _, Multi([])) -> false 
  | TABS(_, _, Multi([])) -> false 
  | TABS(_, _, Star) -> false
  | _ -> true


let rec size_inter_ty = function 
  | Unknown -> 9999999
  | Star -> 1 
  | Arrow(ty1, ty2) -> size_inter_ty ty1 + size_inter_ty ty2
  | Multi(lty) -> List.fold_left (+) 0 (List.map size_inter_ty lty) 


let rec size_typed_term = function 
  | TVAR(x, Multi(lty)) -> List.fold_left (+) 0 (List.map (fun ty ->  size_typed_term @@ TVAR(x, ty)) lty)
  | TVAR(_, ty) -> size_inter_ty ty
  | TAPP(t1, t2, Multi(lty)) -> List.fold_left (+) 0 (List.map (fun ty ->  size_typed_term @@ TAPP(t1, t2, ty)) lty)
  | TAPP(t1, t2, ty) -> size_typed_term t1 + size_typed_term t2 + size_inter_ty ty

  | TABS(_,_,Star) -> 0
  | TABS(x, t, Multi(lty)) -> List.fold_left (+) 0 (List.map (fun ty ->  size_typed_term @@ TABS(x, t, ty)) lty)
  | TABS(_, t, ty) -> size_typed_term t +  size_inter_ty ty

  | TMulti(tlist, _) -> List.fold_left (+) 0 (List.map size_typed_term tlist)

(* printing functions*)

let print_weak_head_run out (lt : term list) = 
  List.iter (fun t -> Printf.fprintf out "%s\n" (sprint_term t);) lt 

let print_weak_head_res out (lt : term list) = 
  Printf.fprintf out "nb beta head=%d\n" (List.length lt)



let rec sprint_type (ty : inter_ty) = match ty with 
  | Star -> "★"
  | Arrow(ty1, ty2) -> (sprint_type ty1) ^ "→"^ (sprint_type ty2)
  | Multi(lty) -> "["^ sprint_multi_type lty ^ "]"
  | Unknown -> "?" 

and sprint_multi_type (lty : inter_ty list) = match lty with 
  | [] -> ""
  | [ty] -> sprint_type ty
  | ty::lty -> sprint_type ty ^ ", " ^ sprint_multi_type lty

let rec sprint_type_latex (ty : inter_ty) = match ty with 
  | Star -> "\\star"
  | Arrow(ty1, ty2) -> (sprint_type_latex ty1) ^ "\\to"^ (sprint_type_latex ty2)
  | Multi(lty) -> "["^ sprint_multi_type_latex lty ^ "]"
  | Unknown -> "?" 

and sprint_multi_type_latex (lty : inter_ty list) = match lty with 
  | [] -> ""
  | [ty] -> sprint_type_latex ty
  | ty::lty -> sprint_type_latex ty ^ ", " ^ sprint_multi_type_latex lty

let rec sprint_no_typing2 = function 
  | TVAR(x,_) -> x 

  | TABS(x, t, _) -> Printf.sprintf "λ%s.%s" x (sprint_no_typing2 t)

  | TAPP(TVAR(x,_), TVAR(y,_), _) -> Printf.sprintf "%s %s" x y
  | TAPP(TAPP(_,_,_) as t1, TVAR(y,_), _) -> Printf.sprintf "%s %s" (sprint_no_typing2 t1) y
  | TAPP(t1, TVAR(y,_), _) -> Printf.sprintf "(%s) %s" (sprint_no_typing2 t1) y
  | TAPP(TAPP(_,_,_) as t1, t2, _) -> Printf.sprintf "%s (%s)" (sprint_no_typing2 t1) (sprint_no_typing2 t2)
  | TAPP(t1, t2, _) -> Printf.sprintf "(%s) (%s)" (sprint_no_typing2 t1) (sprint_no_typing2 t2)

  | TMulti(tlist, _) -> Printf.sprintf "{%s|}" (List.fold_left (fun acc s -> acc ^ "|"^ s) "" (List.map sprint_no_typing2 tlist))

let sprint_no_typing t = sprint_term (remove_types t) 


let rec sprint_typing (t : typed_term) = match t with 
  | TVAR(x, ty) -> Printf.sprintf "%s:%s" x (sprint_type ty)
  | TAPP(t1, t2, Multi([])) -> Printf.sprintf "%s : []" (sprint_no_typing (TAPP(t1, t2, Multi([]))))
  | TABS(x, t, Multi([])) -> Printf.sprintf "%s : []" (sprint_no_typing (TABS(x, t, Multi([]))))
  | TAPP(TVAR(y, tyy), TVAR(x, tyx), ty) -> Printf.sprintf "(%s %s):%s" (sprint_typing (TVAR(y, tyy))) (sprint_typing (TVAR(x, tyx))) (sprint_type ty)
  | TAPP(t1, TVAR(x, tyx), ty) -> Printf.sprintf "((%s) %s):%s" (sprint_typing t1) (sprint_typing (TVAR(x, tyx))) (sprint_type ty)
  | TAPP(t1, t2, ty) -> Printf.sprintf "((%s) (%s)):%s" (sprint_typing t1) (sprint_typing t2) (sprint_type ty)

  | TABS(x, t, Star) -> Printf.sprintf "(λ%s.%s):%s" x (sprint_no_typing t) (sprint_type Star)
  | TABS(x, t, ty) -> Printf.sprintf "(λ%s.%s):%s" x (sprint_typing t) (sprint_type ty)
  | TMulti([], _) -> Printf.sprintf "E"
  | TMulti(lt, _) -> Printf.sprintf "{%s|}" (List.fold_left (fun acc s -> acc ^ "|"^ s) "" (List.map sprint_typing lt))


  (* | TAPP(TVAR(y, tyy), TVAR(x, tyx), _) -> Printf.sprintf "%s %s" (sprint_typing (TVAR(y, tyy))) (sprint_typing (TVAR(x, tyx)))
  | TAPP(t1, TVAR(x, tyx), _) -> Printf.sprintf "(%s) %s" (sprint_typing t1) (sprint_typing (TVAR(x, tyx)))
  | TAPP(TAPP(t11, t12, ty), t2, _) -> Printf.sprintf "%s (%s)" (sprint_typing (TAPP(t11, t12, ty))) (sprint_typing t2)
  | TAPP(t1, t2, _) -> Printf.sprintf "(%s) (%s)" (sprint_typing t1) (sprint_typing t2) *)

let print_typing out (t : typed_term) = Printf.fprintf out "%s\n" (sprint_typing t)

let print_typing_res out (t:typed_term) = Printf.fprintf out "size typing=%d\n" (size_typed_term t)

let rec sprint_typing_latex (t : typed_term) = match t with 
  | TVAR(x, Multi([ty])) -> 
    Printf.sprintf "\\AXC{$\\vdash %s : %s$}\n \\UIC{$\\vdash %s : \\color{red}%s$}" x (sprint_type_latex ty) x (sprint_type_latex (Multi([ty])))
  | TVAR(x, ty) -> 
    Printf.sprintf "\\AXC{$\\vdash %s : %s$}" x (sprint_type_latex ty)


  | TAPP(t1, t2, Multi([])) -> 
    Printf.sprintf "\\AXC{$\\vdash %s : %s$}" (sprint_no_typing (TAPP(t1, t2, Multi([])))) (sprint_type_latex (Multi([])))
  | TAPP(t1, t2, ty) -> 
    let s1 = sprint_typing_latex t1 in 
    let s2 = sprint_typing_latex t2 in 
    Printf.sprintf "%s\n%s\n\\RL{T-@}\n\\BIC{$\\vdash %s : %s$}" s1 s2 (sprint_no_typing (TAPP(t1, t2, ty))) (sprint_type_latex ty)


  | TABS(x, t, Star) -> 
    Printf.sprintf "\\AXC{}\n\\RL{T-$\\star$}\n \\UIC{$\\vdash %s : \\color{red}%s$}" (sprint_no_typing (TABS(x, t, Star))) (sprint_type_latex Star)
  | TABS(x, t, Multi([])) -> 
    Printf.sprintf "\\AXC{$\\vdash %s : %s$}" (sprint_no_typing (TABS(x, t, Star))) (sprint_type_latex (Multi([])))
  (* | TABS(x, t, Multi([lty])) -> 
    Printf.sprintf "\\AXC{$\\vdash %s : %s$}" (sprint_no_typing (TABS(x, t, Star))) (sprint_type_latex (Multi([]))) *)
  | TABS(x, t, ty) -> 
    let s1 = sprint_typing_latex t in 
    Printf.sprintf "%s\n\\RL{T-$\\lambda$}\\UIC{$\\vdash %s : %s$}" s1 (sprint_no_typing (TABS(x, t, ty))) (sprint_type_latex ty)

  | TMulti([], _) -> ""
  | TMulti([x], lty) -> 
    let s1 = sprint_typing_latex x in
    let s2 = sprint_multi_type_latex lty in
    Printf.sprintf "%s\n\\RL{T-M}\n\\UIC{$\\vdash %s : \\color{red}[%s]$}" s1 (sprint_no_typing t) s2
  | TMulti(x::[y], lty) -> 
    let sx = sprint_typing_latex x in
    let sy = sprint_typing_latex y in
    (* Printf.printf "x and y are differents??\n%s\n" (sprint_no_typing @@ TMulti(x::[y], lty)); *)
    let s2 = sprint_multi_type_latex lty in
    Printf.sprintf "%s\n%s\n\\RL{T-M}\n\\BIC{$\\vdash %s : \\color{red}[%s]$}" sx sy (sprint_no_typing t) s2
  | TMulti(x::y::[z], lty) -> 
    let sx = sprint_typing_latex x in
    let sy = sprint_typing_latex y in
    let sz = sprint_typing_latex z in
    let s2 = sprint_multi_type_latex lty in
    Printf.sprintf "%s\n%s\n%s\n\\RL{T-M}\n\\TIC{$\\vdash %s : \\color{red}[%s]$}" sx sy sz(sprint_no_typing t) s2
  | TMulti(x::_, lty) -> 
    let s1 = sprint_typing_latex x in
    let s2 = sprint_multi_type_latex lty in
    let s3 = "\\AXC{\\color{green}...}" in
    Printf.sprintf "%s\n%s\n\\RL{T-M}\n\\BIC{$\\vdash %s : \\color{red}[%s]$}" s1 s3 (sprint_no_typing t) s2
let print_typing_latex out (t : typed_term) = 
  Printf.fprintf out "\\begin{prooftree}\n%s\n\\end{prooftree}" (sprint_typing_latex t)



(* transitions *)
let rec subst t x u = match t with 
  | VAR(y) when String.equal x y -> u 
  | VAR(y) -> VAR(y) 
  | APP(t1, t2) -> APP(subst t1 x u, subst t2 x u) 
  | ABS(y,t) when String.equal x y -> ABS(y,t)
  | ABS(y,t) -> ABS(y, subst t x u)

let rec weak_head = function
  | APP(ABS(x,t), u) -> subst t x u
  | APP(t, u) -> APP(weak_head t, u)
  | t -> t

let rec is_final = function 
  | APP(ABS(_,_), _) -> false
  | APP(t, _) -> is_final t
  | _ -> true 

let rec weak_head_loop (t : term) (res : term list) = 
  if (is_final t) then 
    t::res 
  else
    weak_head_loop (weak_head t) (t :: res)


let weak_head_run (t : term) = weak_head_loop t []
let concat_pair l1 l2 =
    let (a1, b1) = l1 in 
    let (a2, b2) = l2 in 
    (a1 @ a2, b1 @ b2)


let rec collect (next : typed_term) (t : term) (x : ident) : typed_term list * inter_ty list = 
  match next, t with  
  | next , VAR(y) when var_equal y x -> ([next] , [type_of next])
  | _ , VAR(_) -> ([], [])
  | next , _ when not @@ should_collect_bellow next -> ([], [])
  | TAPP(nt1, nt2, _), APP(t1, t2) -> concat_pair (collect nt1 t1 x) (collect nt2 t2 x)
  | TABS(_, _, _), ABS(y, _) when var_equal y x -> ([], [])
  | TABS(_, nt,_), ABS(_, t) -> collect nt t x
  | TMulti(lnt, _), t -> 
    let tmp = List.map (fun next -> collect next t x) lnt in 
    List.fold_left concat_pair ([], []) tmp
  | _, _ -> 
    Printf.printf "%s != %s" (sprint_no_typing next) (sprint_term t);
    failwith "next and t are different??"

let rec copy_ty (next : typed_term) (t : term) : typed_term =
  match next, t with 
  | TVAR(x, ty) , VAR(_) -> TVAR(x, ty)
  | next , VAR(y) -> TVAR(y, type_of next)
  | TABS(y, next, ty), ABS(_, t) -> TABS(y, copy_ty next t, ty)
  | TAPP(next1, next2, ty), APP(t1, t2) -> TAPP(copy_ty next1 t1, copy_ty next2 t2, ty)
  | TMulti(lnt, lty), t ->  TMulti(List.map (fun next -> copy_ty next t) lnt, lty)
  | _, _ -> 
    Printf.printf "%s != \n %s" (sprint_no_typing next) (sprint_term t);
    failwith "next and t are different??"






(* takes a term t and the type of the reduced term u when t -> u
   return the type version t*)
let rec subject_expansion (t : term) (next : typed_term) : typed_term =
  (* Printf.printf "next=%s\nt=%s\n\n" (sprint_no_typing next) (sprint_term t);  *)
  match next with 
  (* if next is an ABS, then the redex is at toplevel in t *)
  | TABS(_,_,Star) -> 
    begin match t with 
    (* if there is another ABS, we can type u with [] *)
    | APP(ABS(x,ABS(y, t)), u) -> 
      let tty = Arrow(Multi([]), Star) in 
      let typed_u = ty_toplevel u (Multi([])) in 
      let typed_t = ty_toplevel (ABS(y, t)) Star in
      TAPP(TABS(x,typed_t,tty), typed_u , Star)

    (* else the only other case is x (if there is an APP then next is not a normal form) *)
    | APP(ABS(x,VAR(y)), u) when String.equal x y ->
      let tty = Arrow(Multi([Star]), Star) in 
      let typed_u = TMulti([ty_toplevel u Star], [Star]) in 
      let typed_t = TVAR(y, Star) in
      TAPP(TABS(x,typed_t,tty), typed_u , Star)
    | _ -> failwith "can't happen if the conditions are met"
    end

  | TABS(_, _, ty) -> 
    begin match t with 
    (* if there is another ABS, we can type u with [] *)
    | APP(ABS(x,t), u) -> 
      let (ulist, tylist) = collect next t x in 
      let tty = Arrow(Multi(tylist), ty) in 
      let typed_u = if List.is_empty ulist then ty_toplevel u (Multi([])) else TMulti(ulist, tylist) in 
      let typed_t = copy_ty next t in
      TAPP(TABS(x, typed_t, tty), typed_u , ty)

    | _ -> failwith "can't happen if the conditions are met"
    end

  | TAPP(nextt, nextu, ty) -> 
    begin match t with 
    | APP(ABS(x,t), u) -> 
      let (ulist, tylist) = collect next t x in 
      let tty = Arrow(Multi(tylist), ty) in 
      let typed_u = if List.is_empty ulist then ty_toplevel u (Multi([])) else TMulti(ulist, tylist) in 
      let typed_t = copy_ty next t in
      TAPP(TABS(x, typed_t, tty), typed_u, ty)
    
    | APP(t, _) ->  (* we must have u == nextu *)
      TAPP(subject_expansion t nextt, nextu, ty)
    
    | _ -> failwith "can't happen if the conditions are met"
    end
  | _ -> failwith "Not Closed? Not my problem!"

let rec type_run (lt : term list) (typed_t : typed_term) = match lt with 
  | [] -> typed_t 
  | t::lt -> type_run lt (subject_expansion t typed_t)

let type_run (lt : term list) = match lt with 
  | [] -> failwith "cannot type an empty run"
  | t::lt -> type_run lt (ty_toplevel t Star)
