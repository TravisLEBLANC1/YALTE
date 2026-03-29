%{
    open Lexing
    open Yaltelib.Types
%}

%token EOF
%token <string> IDENT
%token LAMBDA DOT EQ

%token LET REC IN
%token LPAR RPAR


%start program
%type <Yaltelib.Types.prog> program 
%type <Yaltelib.Types.letbind> bindings 
%type <Yaltelib.Types.letbinds> list(bindings)
%type <Yaltelib.Types.term> term
%type <Yaltelib.Types.term> app_term 
%type <Yaltelib.Types.term> atom
%%
    
program:
| bindings=list(bindings) t=term EOF { {bindings = bindings; term = t} }
;

bindings:
| LET x=IDENT EQ t=term IN { LET(x, t) }
| LET REC x=IDENT EQ t=term IN { LETREC(x, t) }
;

term: 
| LAMBDA x=IDENT DOT t=term { ABS(x, t) }
| app=app_term  { app }  // we need to split app for the shift/reduce conflict

app_term:
| a=atom { a }
| t1=app_term a=atom { APP(t1, a)} // left associativity is enforced here thanks to left recursion of app_term

atom:
| x=IDENT { VAR(x) }
| LPAR t=term RPAR { t }