
(* The type of tokens. *)

type token = 
  | WHILE
  | VOID
  | SUPEQ
  | SUP
  | SET
  | SEMI
  | RPAR
  | RETURN
  | PUTCHAR
  | OR
  | NEG
  | MUL
  | LPAR
  | INT
  | INFEQ
  | INF
  | IF
  | IDENT of (string)
  | EQUAL
  | EOF
  | END
  | ELSE
  | DIV
  | CST of (int)
  | COMMA
  | BOOL_CST of (bool)
  | BOOL
  | BEGIN
  | AND
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Minic_ast.prog)
