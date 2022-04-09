(* Représentation des types. *)
type typ =
  | Int
  | Bool
  | Void

(* Représentation des expressions.
   Ajouté : les constantes booléennes. *)
type expr =
  | Cst of int
  | BCst of bool
  | Unit of unit
  | Get of string
  | Call of string * expr list
  | Sup of expr * expr 
  | SupEq of expr * expr 
  | Inf of expr * expr 
  | InfEq of expr * expr 
  | Equal of expr * expr 
  | Add of expr * expr 
  | Neg of expr 
  | Div of expr * expr 
  | Mul of expr * expr 
  | Sub of expr * expr 
  | And of expr * expr 
  | Or of expr * expr

(* Représentation des instructions et séquences. *)
type instr =
  | Putchar of expr
  | Set of string * expr
  | If  of expr * seq * seq
  | While of expr * seq
  | Return of expr
  | Expr of expr
and seq = instr list

(* Représentétion des fonctions. *)
type fun_def = {
  name: string;
  params: (string * typ) list;
  return: typ;
  locals: (string * typ * expr) list;
  code: seq;
}


(* Représentation des programmes.
   En réponse à l'indication de l'énoncé, j'associe une valeur entière
   à chaque variable globale. Mais vous voudrez peut-être faire évoluer
   cela (et procéder de même pour les variables locales des fonctions). *)
type prog = {
  globals: (string * typ * expr) list;
  functions: fun_def list;
}

type value = 
  | VCst of int 
  | VBool of bool 
  | VUnit of unit 
and env = (string,value) Hashtbl.t 