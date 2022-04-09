open Minic_ast
(* Pour représenter les environnements associant chaque variable à son type. *)
module Env = Map.Make(String)

(* Vérification du bon typage d'un programme. *)
let typecheck_program (prog: prog) =
  (* L'environnement global mémorise le type de chaque variable globale. *)
  let global_env =
    List.fold_left (fun env (x, ty, _) -> Env.add x ty env) Env.empty prog.globals
  in

  (* Vérification du bon typage d'une fonction.
     C'est une fonction locale : on a accès à [prog] et à [global_env]. *)
  let typecheck_function (fdef: fun_def) =
    
    (* On devrait ici avoir également un environnement local.
       À COMPLÉTER
     *)
    let local_env =
		List.fold_left (fun env (s, t, _) -> Env.add s t env) global_env fdef.locals (*union des var locales et de l'env global*)
	in
    (* Vérification du bon typage et calcul du type d'une expression.
       À nouveau, fonction locale avec accès à tout ce qui est au-dessus. *)
    let rec type_expr = function
      | Cst _ -> Int
      | BCst _ -> Bool
      | Sup(e1,e2) | SupEq(e1,e2) | Inf(e1,e2) | InfEq(e1,e2) | Equal(e1,e2) -> 
      let t1 = type_expr e1 in
      let t2 = type_expr e2 in 
      if t1 != Int || t2 != Int then failwith "type error"
      else Bool 
      | Add(e1,e2) | Mul(e1,e2) | Sub(e1,e2) | Div(e1,e2) ->
      let t1 = type_expr e1 in
      let t2 = type_expr e2 in 
      if t1 != Int || t2 != Int then failwith "type error"
      else Int 
      | Or(e1,e2) | And(e1,e2) ->
      let t1 = type_expr e1 in 
      let t2 = type_expr e2 in 
      if t1 != Bool || t2 != Bool then failwith "type error"
      else Bool 
      | Neg(e1) -> 
      if type_expr e1 != Int then failwith "type error"
      else Int 
      | Get(x) -> Env.find x local_env
	  |Call(f, p) -> let list_prog = List.map (fun a -> a.name) prog.functions in (*on récupère les noms de fonction du programme*)
		if not (List.mem f list_prog) then begin Printf.printf "%s" f; failwith "Undeclared function"; end
		else
		let fprog = List.find ((fun f func -> func.name = f) f) prog.functions in (*cherche la bonne fonction parmis celle du programme*)
		let list_typ_param_appelant = List.map (fun a -> type_expr a) p in (*génère une liste de types des arguments*)
		let list_typ_param_appele = List.map (fun a -> snd(a)) fprog.params in (*génère une liste de types des paramètres de la fonction appelé*)
		if (List.for_all2 (fun app env -> app = env) list_typ_param_appelant list_typ_param_appele) then fprog.return else failwith "Incorrect argument type"
	in
    (* Vérification du bon typage d'une instruction ou d'une séquence.
       Toujours local. *)
    let rec typecheck_instr = function
      (* Cas d'une instruction [return]. On vérifie que le type correspond au
         type de retour attendu par la fonction dans laquelle on se trouve. *)
      | Return(e) -> 
        let t = type_expr e in
        if t <> fdef.return then failwith "type error"
      | Putchar(e) -> 
        let t = type_expr e in 
        if t <> Int then failwith "type error"
      | While(e,li) -> 
        let t = type_expr e in
        if t <> Bool then failwith "type error"
        else List.iter typecheck_instr li;
      | If(e,li1,li2) -> 
        let t = type_expr e in 
        if t <> Bool then failwith "type error"
        else List.iter typecheck_instr li1;
        List.iter typecheck_instr li2;
	|Set(x, e) -> if not (Env.mem x local_env) then begin Printf.printf "%s : " x; failwith "Undeclared variable"; end
		else
		let t1 = type_expr e in 
		let t2 = Env.find x local_env in
		if t1 <> t2 then failwith "type error"
		
    and typecheck_seq s =
      List.iter typecheck_instr s        
    in

    (* Code principal du typage d'une fonction : on type ses instructions. *)
    typecheck_seq (fdef.code);
  in
  (* Code principal du typage d'un programme : on type ses fonctions.
     Il faudrait aussi vérifier les valeurs initiales des variables globales.
     À COMPLÉTER
   *)
  List.iter typecheck_function (prog.functions);
  