open Term
open Print


type tape = 
  | TNil
  | Bullet of tape 
  | Lpos of lpos * tape
and lpos = ident * context * log
and log = 
  | LNil 
  | LCons of lpos * log

type dir = Up | Down

type trans_name = Trans_None | Trans_Var | Trans_Arg | Trans_B1 | Trans_B2 | Trans_B3 | Trans_B4 | Trans_bt1 | Trans_bt2

type config = Conf of term * context * log * tape * dir * trans_name


let rec sprint_tape (t : tape) = match t with 
  | TNil -> "ε"
  | Bullet(TNil) -> "•"
  | Bullet(t) -> "•" ^ sprint_tape t 
  | Lpos(l, TNil) -> sprint_lpos l
  | Lpos(l, t) -> Printf.sprintf "%s%s" (sprint_lpos l) (sprint_tape t)

and sprint_lpos ((x, ctxt, log) : lpos) = 
  Printf.sprintf "(%s, %s, %s)" x (sprint_ctxt ctxt) (sprint_log log) 
and sprint_log (log : log) = match log with 
  | LNil -> "ε"
  | LCons(l, LNil) -> sprint_lpos l
  | LCons(l, log) -> Printf.sprintf "%s::%s" (sprint_lpos l) (sprint_log log)

let sprint_dir (dir : dir) = match dir with 
  | Up -> "↑"
  | Down -> "↓"

let sprint_trans_name (trans_name) = match trans_name with 
  | Trans_None -> ""
  | Trans_Var -> "→var"
  | Trans_Arg -> "→arg"
  | Trans_B1 -> "→•1"
  | Trans_B2 -> "→•2"
  | Trans_B3 -> "→•3"
  | Trans_B4 -> "→•4"
  | Trans_bt1 -> "→bt1"
  | Trans_bt2 -> "→bt2"

let sprint_config (Conf(t, ctxt, log, tape, dir, trans_name) : config) = 
  [| sprint_trans_name trans_name; sprint_term t ; sprint_ctxt ctxt ; sprint_log log ; sprint_tape tape ; sprint_dir dir|]

let print_config (c : config) = 
  let s = sprint_config c in
  for i = 0 to 4 do 
    Printf.printf "%s | " (s.(i))
  done;
  print_newline () 

let fprint_config out (c : config) = 
  let s = sprint_config c in
  for i = 0 to 4 do 
    Printf.fprintf out "%s | " (s.(i))
  done;
  print_newline () 


let print_iam_run out (run : config list) = 
  let header : string array = [| "Trans"; "Term"; "Ctxt"; "Log" ; "Tape" ; "Dir" |] in
  let configs = Array.of_list (List.map sprint_config run) in
  Print.print_run out header configs


let count_trans (run : config list) (tr : trans_name) = 
  List.fold_left (fun n (Conf(_,_,_,_,_,tr')) -> if tr = tr' then n+1 else n) 0 run

let print_iam_result out (run : config list) = 
  let n = (List.length run) in
  (* Printf.fprintf out "%d\n" n *)
  Printf.fprintf out "length=%d bt1=%d\n" n (count_trans run Trans_bt1)
  (* fprint_config out (List.nth run (n-1)) *)


(*** transitions functions ***)

let rec concat_log (l1:log) (l2 : log): log =
  match l1 with 
  | LNil -> l2 
  | LCons(l, l1) -> LCons(l, concat_log l1 l2)

(* return a pair with (the first n elements, the rest) *)
let rec nhead_log (n:int) (log:log) : (log*log) = 
  if (n = 0) then 
    (LNil, log)
  else
    match log with 
    | LNil -> 
        raise (Failure("not enough elements in the log")) 
    | LCons(l, log) -> 
      let (res, log) = nhead_log (n-1) log in 
      (LCons(l, res), log)

let rec tape_make (n:int) = match n with 
  | 0 -> TNil
  | n -> Bullet(tape_make (n-1))

let rec level = function 
  | [] -> 0
  | RAPP(_) :: ctxt -> level ctxt + 1
  | _ :: ctxt -> level ctxt



(* return the next config of the KAM *)
let trans (Conf(t, ctxt, lo, tape, dir, _) : config) : config = 
  (* print_config (Conf(t, ctxt, lo, tape, dir)); *)
  match t, ctxt, lo, tape, dir with 
  | APP(u, t), ctxt, lo, tape, Down                     -> (* →•1  *) Conf(u, LAPP(t)::ctxt, lo, Bullet(tape), Down, Trans_B1)
  | ABS(x, t), ctxt, lo, Bullet(tape), Down             -> (* →•2  *) Conf(t, CABS(x)::ctxt, lo, tape, Down, Trans_B2)
  | ABS(_, _), ctxt, lo, Lpos((x, c, lo'), tape), Down  -> (* →bt2 *) Conf(VAR(x), c @ ctxt, concat_log lo' lo, tape, Up, Trans_bt2) (* TODO sanity check?*)

  | u, LAPP(t)::ctxt, lo, Bullet(tape), Up              -> (* →•3  *) Conf(APP(u, t), ctxt, lo, tape, Up, Trans_B3)
  | t, CABS(x)::ctxt, lo, tape, Up                      -> (* →•4  *) Conf(ABS(x, t), ctxt, lo, Bullet(tape), Up, Trans_B4)
  | u, LAPP(t)::ctxt, lo, Lpos(l, tape), Up             -> (* →arg *) Conf(t, RAPP(u)::ctxt, LCons(l, lo), tape, Down, Trans_Arg)
  | t, RAPP(u)::ctxt, LCons(l, lo), tape, Up            -> (* →bt1 *) Conf(u, LAPP(t)::ctxt, lo, Lpos(l, tape), Down, Trans_bt1)

  | VAR(x), ctxt, lo, tape, Down                        ->  (* →var *)
    let octxt = find_abs x ctxt in  
    if Option.is_none octxt then 
      Conf(t, ctxt, lo, tape, dir, Trans_None) (*final state (open variable)*)
    else
      let (dctxt, ctxt) = Option.get octxt in
      let (lon, lo) = nhead_log (level dctxt) lo in
                                                          Conf(fill_hole dctxt (VAR(x)), ctxt, lo, Lpos((x, dctxt, lon) ,tape), Up, Trans_Var)
  | _ -> Conf(t, ctxt, lo, tape, dir, Trans_None) (* final state*)

let is_final (Conf(t, ctxt, lo, tape, dir,_) : config) =
  match t, ctxt, lo, tape, dir with 
  | ABS(_, _), _, _, TNil, Down -> true 
  | _, [], _, _, Up -> true 
  | VAR(x), _, _, _, Down when (Option.is_none (find_abs x ctxt)) -> true 
  | _ -> false 

let rec iam_loop (c : config) : config list = 
  if is_final c then
    [c]
  else
    c :: iam_loop (trans c)

let rec iam_loop_noverbose (c : config) : config list = 
  if is_final c then 
    [c]
  else
    iam_loop_noverbose (trans c)

let iam (t : term) (n : int) (_ : bool): config list = 
  let conf = Conf(t, [], LNil, tape_make n, Down, Trans_None) in  
  iam_loop conf

