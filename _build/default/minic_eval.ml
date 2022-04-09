open Minic_ast
(* Pour représenter les environnements associant chaque variable à son type. *)
let rec eval_expr (e:expr) (env:env):value = 
  match e with 
    | Cst n -> VCst n 
    | BCst n -> VBool n
    | Unit n -> VUnit n
    | Add(e1,e2) ->
      let v1 = eval_expr e1 env in 
      let v2 = eval_expr e2 env in 
      let n = 
        match v1,v2 with
        | VCst n1, VCst n2 -> n1+n2
        | _ -> assert false 
      in VCst n
    | Sup(e1,e2) -> 
      let v1 = eval_expr e1 env in 
      let v2 = eval_expr e2 env in 
      let n = 
        match v1,v2 with
          | VCst n1, VCst n2 -> n1 > n2
          | _ -> assert false 
      in VBool n 
    | SupEq(e1,e2) ->
      let v1 = eval_expr e1 env in 
      let v2 = eval_expr e2 env in 
      let n = 
        match v1,v2 with
          | VCst n1, VCst n2 -> n1 >= n2
          | _ -> assert false 
      in VBool n 
    | Inf(e1,e2) ->
      let v1 = eval_expr e1 env in 
      let v2 = eval_expr e2 env in 
      let n = 
        match v1,v2 with
          | VCst n1, VCst n2 -> n1 < n2
          | _ -> assert false 
      in VBool n 
    | InfEq(e1,e2) ->
      let v1 = eval_expr e1 env in 
      let v2 = eval_expr e2 env in 
      let n = 
        match v1,v2 with
          | VCst n1, VCst n2 -> n1 <= n2
          | _ -> assert false 
      in VBool n 
    | Equal(e1,e2) ->
      let v1 = eval_expr e1 env in 
      let v2 = eval_expr e2 env in 
      let n = 
        match v1,v2 with
          | VCst n1, VCst n2 -> n1 == n2
          | _ -> assert false 
      in VBool n 
    | Div(e1,e2) ->
      let v1 = eval_expr e1 env in 
      let v2 = eval_expr e2 env in 
      let n = 
        match v1,v2 with
          | VCst n1, VCst n2 -> n1 / n2
          | _ -> assert false 
      in VCst n 
    | Mul(e1,e2) ->
      let v1 = eval_expr e1 env in 
      let v2 = eval_expr e2 env in 
      let n = 
        match v1,v2 with
          | VCst n1, VCst n2 -> n1 * n2
          | _ -> assert false 
      in VCst n 
    | Sub(e1,e2) ->
      let v1 = eval_expr e1 env in 
      let v2 = eval_expr e2 env in 
      let n = 
        match v1,v2 with
          | VCst n1, VCst n2 -> n1 - n2
          | _ -> assert false 
      in VCst n 
    | And(e1,e2) ->
      let v1 = eval_expr e1 env in 
      let v2 = eval_expr e2 env in 
      let n = 
        match v1,v2 with
          | VBool n1, VBool n2 -> n1 && n2
          | _ -> assert false 
      in VBool n 
    | Or(e1,e2) ->
      let v1 = eval_expr e1 env in 
      let v2 = eval_expr e2 env in 
      let n = 
        match v1,v2 with
          | VBool n1, VBool n2 -> n1 || n2
          | _ -> assert false 
      in VBool n 
    | Neg(e1) -> 
        let v = eval_expr e1 env in
        let n = 
          match v with
            |VCst n -> -n 
            | _ -> assert false 
        in VCst n 

let rec eval_instruction (i:instr) (env: env):unit = 
  match i with 
  | Set(x,e) -> 
    let v = eval_expr e env in 
    Hashtbl.replace env x v
  | If(e,i1,i2) ->
    let v = eval_expr e env in 
    if v = VBool true then eval_instruction2 i1 env 
    else eval_instruction2 i2 env 
  | While(e,i) ->
    let v = eval_expr e env in 
    if v <> VBool true then eval_instruction2 i env
  and eval_instruction2 (i:instr list) (env: env): unit = 
    match i with 
      | [] -> ()
      | i::i' -> eval_instruction i env; eval_instruction2 i' env