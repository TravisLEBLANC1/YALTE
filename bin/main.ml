open Parser 
open Lexing
open Format
open Yaltelib

let usage = "usage: ./main.exe [-iam/-kam] prog.ml"

let verbose_flag = ref false
let iam_flag = ref false 
let kam_flag = ref false 

let spec = [
  ("-v", Arg.Set verbose_flag, "print the entire run");
  ("-iam", Arg.Set iam_flag, "enable iam run");
  ("-kam", Arg.Set kam_flag, "enable kam run");
]

let report filename (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" filename l fc lc
    

let parse filename parsefun lb = 
  try 
    parsefun Lexer.token lb  
  with
      | Parser.Error -> report filename (lexeme_start_p lb, lexeme_end_p lb);
    eprintf "syntax error@.";
    exit 1


let file =
    let file = ref None in
    let set_file s =
      file := Some s
    in
    Arg.parse spec set_file usage;
    match !file with Some f -> f | None -> Arg.usage spec usage; exit 1


let compute_term term = 
  if !iam_flag then 
    let iam_run = Iam.iam term 0 in
    if !verbose_flag then
      Iam.print_iam_run stdout iam_run
    else
      Iam.print_iam_result stdout iam_run;
  if !kam_flag then  
    let kam_run = Kam.kam term in
    if !verbose_flag then
      Kam.print_kam_run stdout kam_run
    else
      Kam.print_kam_result stdout kam_run
  else 
    Printf.printf "%s\n" usage

let () =
  let c_prog  = open_in file in
  let lb_prog = Lexing.from_channel c_prog in
  let prog = parse file Parser.program lb_prog in 
  let terms = Term.prog_to_term prog in
  List.iter compute_term terms