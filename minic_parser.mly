%{
   open Lexing
   open Minic_ast

   let default_value = function
      | Int -> Cst(0)
      | Bool -> BCst(false)
	  | Void -> Unit()
%}

(* Déclaration des lexèmes *)
%token <int> CST
%token <bool> BOOL_CST
%token <string> IDENT
%token LPAR RPAR BEGIN END
%token RETURN SET SEMI COMMA 
%token INT BOOL VOID
%token EOF
%token SUP SUPEQ INF INFEQ EQUAL
%token ADD DIV MUL NEG
%token AND OR
%token PUTCHAR WHILE IF ELSE

%nonassoc NEG
%nonassoc SUP SUPEQ INF INFEQ EQUAL 
%nonassoc OR AND
%left ADD DIV MUL

%start program
%type <Minic_ast.prog> program

%%

(* Un programme est une liste de déclarations.
   On ajoute une règle déclenchée en cas d'erreur, donnant une
   information minimale : la position. *)
program:
| dl=declaration_list EOF
       { let var_list, fun_list = dl in
         { globals = var_list; functions = fun_list; } }
| error { let pos = $startpos in
          let message =
            Printf.sprintf
              "Syntax error at %d, %d"
              pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
          in
          failwith message }
;

(* Chaque déclaration peut concerner une variable ou une fonction. *)
declaration_list:
| { [], [] }    
| vd=variable_decl dl=declaration_list { let vl, fl = dl in vd :: vl, fl }
| fd=function_decl dl=declaration_list { let vl, fl = dl in vl, fd :: fl }
;

(* Déclaration de variable.
   Note : on ne traite ici que le cas où une valeur initiale est fournie. *)
variable_decl:
| t=typ x=IDENT SET n=expression SEMI { (x, t, n) }
| t=typ x=IDENT SEMI { (x,t,default_value t) }

;

parameter_list:
| t=typ x=IDENT COMMA r=parameter_list {[(x, t)]@r}
| t=typ x=IDENT {[(x, t)]}
;

(* Indication de type.*)
typ:
| INT { Int }
| BOOL { Bool }
| VOID { Void }
;

(* Déclaration de blocs d'instructions.
Note:ils contiennent des variables locales à initialiser ou sinon elles le sont par défaut et
une suite d'instructions.
*)
(*bloc_instruc:
| BEGIN l=list(variable_decl) s=list(instruction) END { { cod=s; loc=l } }*)

(* Déclaration de fonction.*)
function_decl:
| t=typ f=IDENT LPAR RPAR BEGIN v=list(variable_decl) s=list(instruction) END (*cas sans param*)
{ { name=f; code=s; params=[]; return=t; locals=v} }
| t=typ f=IDENT LPAR p=parameter_list RPAR BEGIN v=list(variable_decl) s=list(instruction) END
{ { name=f; code=s; params=p; return=t; locals=v} }
;

arg_list:
|e=expression COMMA r=arg_list {[e]@r}
|e=expression {[e]}
;

(* Instructions.*)
instruction:
| RETURN e=expression SEMI {Return(e)}
| PUTCHAR LPAR e=expression RPAR SEMI {Putchar(e)} 
| WHILE LPAR e=expression RPAR BEGIN i=list(instruction) END {While(e,i)}
| IF LPAR e=expression RPAR BEGIN i1=list(instruction) END ELSE BEGIN  i2=list(instruction) END {If(e,i1,i2)}
| IF LPAR e=expression RPAR BEGIN i1=list(instruction) END {If(e,i1,[])}
| x=IDENT SET e=expression SEMI {Set(x,e)}
;

(* Expressions. *)
expression:
| x=IDENT {Get(x)}
| n=CST { Cst(n) }
| b=BOOL_CST { BCst(b) }
| e1=expression SUP e2=expression {Sup(e1,e2)}
| e1=expression SUPEQ e2=expression {SupEq(e1,e2)}
| e1=expression INF e2=expression {Inf(e1,e2)}
| e1=expression INFEQ e2=expression {InfEq(e1,e2)}
| e1=expression ADD e2=expression {Add(e1,e2)}
| NEG e1=expression {Neg(e1)}
| e1=expression NEG e2=expression {Sub(e1,e2)}
| e1=expression MUL e2=expression {Mul(e1,e2)}
| e1=expression DIV e2=expression {Div(e1,e2)}
| e1=expression OR e2=expression {Or(e1,e2)}
| e1=expression AND e2=expression {And(e1,e2)}
| e1=expression EQUAL e2=expression {Equal(e1,e2)}
| f=IDENT LPAR p=arg_list RPAR {Call(f, p)}
| f=IDENT LPAR RPAR {Call(f, [])}
;
