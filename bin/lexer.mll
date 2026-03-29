{
    open Lexing
    open Parser

    let keyword_or_ident =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "let",   LET;
        "rec", REC;
        "in",    IN;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)
}
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | '\'' | digit)*
  
rule token = parse
  | ['\n']
      { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | "(*" 
      { comment lexbuf; token lexbuf }
  | ident as id
      { keyword_or_ident id }
  | "λ" 
      { LAMBDA }
  | "."
      { DOT }
  | "=" 
      { EQ }
  | "("
      { LPAR }
  | ")"
      { RPAR }
  | _
      { failwith ("Unknown character : " ^ (lexeme lexbuf)) }
  | eof
      { EOF }

and comment = parse
  | "*)"
      { () }
  | "(*"
      { comment lexbuf; comment lexbuf }
  | _
      { comment lexbuf }
  | eof
      { failwith "unfinished comment" }