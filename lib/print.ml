open Term

(***** print terms and pure programs ******)
let rec sprint_term = function 
  | VAR(x) -> x 
  | ABS(x, t) -> Printf.sprintf "λ%s.%s" x (sprint_term t)
  | APP(VAR(x), t2) -> Printf.sprintf "%s %s" x (sprint_term t2)
  | APP(t1, VAR(y)) -> Printf.sprintf "(%s) %s" (sprint_term t1) y 
  | APP(t1, t2) -> Printf.sprintf "(%s) (%s)" (sprint_term t1) (sprint_term t2)

let print_bind = function 
  | LET(x, t) -> Printf.printf "let %s = %s in" x (sprint_term t) 
  | LETREC(x, t) -> Printf.printf "let rec %s = %s in" x (sprint_term t) 

let print_bindings = List.iter (fun b -> print_bind b; print_newline ())

let print_term t = print_string (sprint_term t)

let print_program prog : unit = 
  print_bindings prog.bindings;
  print_term prog.term;
  print_newline ()




(**** print abstract machine runs ****)
(* the details will be in each file (kam.ml, iam.ml, ...)
   those are just generic functions to help*)

(* raise an error of the size of the lists are not all equals*)
let check_sizes (n : int) (lss : ('a array) array) =
  Array.iter 
  (fun ls -> if (Array.length ls) != n then raise (Failure("size error in print_run"))) 
  lss

let check_normalize_sizes (ns : int array)  (lss : (string array) array) =
  let length_match (ns : int array) (ls : string array) =
    Array.for_all2 (fun n s -> String.length s = n) ns ls
  in
  Array.iter 
  (fun ls -> if not @@ length_match ns ls then raise (Failure("normalized size error in print_run"))) 
  lss

(* return the max length of each column *)
let max_column (n : int) (lss : (string array) array) : int array = 
  let update_max (max_ls : int array) (ls : string array) : int array = 
    Array.map2 (fun max s -> Int.max (String.length s) max) max_ls ls
  in
  Array.fold_left 
    update_max 
    (Array.init n (fun _ -> 1))
    lss
  
let complete_string s n = 
  let w = Wcwidth.wcswidth s in
  s ^ String.make (max 0 (n - w)) ' '

(* hopefully we call print_line with normalized columns*)
let print_line out (line : string array) =
  Printf.fprintf out "|";
  Array.iter (fun s -> Printf.printf " %s |" s) line;
  Printf.fprintf out "\n"


(* print a table with first the header
  then one config by line *)
  let print_run out (header : string array) (configs : (string array) array) : unit = 
  let n = Array.length header in 
  check_sizes n configs; (* sanity check *)
  
  (* add spaces on each string so that each string in a same column 
     have the same length *)
  let max_col = max_column n (Array.append [| header |] configs) in
  let normalize_line (line:string array) = 
    for i = 0 to n-1 do 
      Array.set line i (complete_string line.(i) max_col.(i))
    done
  in
  Array.iter normalize_line configs;
  normalize_line header;

  (* print each lines *)
  let total_size = Array.fold_left (fun res c -> res + c + 2) 4 max_col in
  let dash_line = String.init total_size (fun _ -> '-') in 
  Printf.fprintf out "%s\n" dash_line;
  print_line out header;
  Printf.fprintf out "%s\n" dash_line;
  Array.iter (print_line out) configs;
  Printf.fprintf out "%s\n" dash_line;

  


