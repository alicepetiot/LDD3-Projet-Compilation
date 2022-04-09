{
  open Lexing
  open Minic_parser

  (* Fonction auxiliaire pour rassembler les mots-clés *)
  let keyword_or_ident =
    let h = Hashtbl.create 40 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "return", RETURN;
        "true", BOOL_CST true;
        "false", BOOL_CST false;
        "int", INT;
        "bool", BOOL;
        "void", VOID;
        "putchar", PUTCHAR;
        "while", WHILE;
		"if", IF;
		"else", ELSE;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)
        
}

(* Règles auxiliaires *)
let digit = ['0'-'9']
let number = digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | '_' | digit)*

(* Règles de reconnaissance *)
rule token = parse
  | ['\n'] { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+ { token lexbuf }
  | number as n { CST(int_of_string n) }
  | ident as id { keyword_or_ident id }
  | ";" { SEMI }
  | "==" { EQUAL } 
  | "=" { SET }
  | "(" { LPAR }
  | ")" { RPAR }
  | "{" { BEGIN }
  | "}" { END }
  | ">=" { SUPEQ }
  | ">" { SUP }
  |"<=" { INFEQ }
  | "<" { INF }
  | "+" { ADD }
  | "-" { NEG }
  | "/" { DIV }
  | "*" { MUL }
  | "&&" { AND }
  | "||" { OR }
  | "," { COMMA }
  | _ { failwith ("Unknown character : " ^ (lexeme lexbuf)) }
  | eof { EOF }
