open Parser 
open Lexing
open Format
open Yaltelib

let usage = "usage: ./yalte prog.ml"

let spec = []

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


let () =
  let c_prog  = open_in file in
  let lb_prog = Lexing.from_channel c_prog in
  let prog = parse file Parser.program lb_prog in 
  let term = Term.prog_to_term prog in
  let kam_run = Kam.kam term in
  Kam.print_kam_run stdout kam_run