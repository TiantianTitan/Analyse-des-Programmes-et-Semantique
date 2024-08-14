open Ast

type prim = 
  Add
  | Sub 
  | Eq
  | Lt
  | Mul
  | Div
  | Not

type value = InZ of int (* Value est équilant à InZ *)
| InF of closure 
| InFR of recClosure
| Operator of prim

and closure = expr * string list * env (* update string *)
and recClosure = expr * string * string list * env
and env = (string * value) list  


let env0 : env = [
  ("true" , InZ(1));
  ("false" , InZ(0));
  ("add" , Operator(Add));
  ("sub" , Operator(Sub));
  ("eq" , Operator(Eq));
  ("lt" , Operator(Lt));
  ("mul" , Operator(Mul));
  ("div" , Operator(Div));
  ("not" , Operator(Not));
]


(* InZ -> int *)
let rec sem_InZ v =
  match v with
  | InZ(i) ->  i
  | _ -> failwith "the value is not type of InZ()"

(* get value par rapport au nom de variable dans l'env*)  
and get_value (id: string) (env:env) =
  match env with
  | [] -> failwith "id is not found in env"
  | hd :: tl -> let (var,value) = hd in
                if var = id then value
                else get_value id tl

  and sem_op_unary op v =
    match op with
    | Operator(Not) ->  if v = InZ(1) then InZ(0)
    else InZ(1)
    | _ -> failwith "Not defined this unary operation"             

  and sem_op_binary op v1 v2 = 
    match op with
    | Operator(Add) -> InZ(v1 + v2)
    | Operator(Sub) -> InZ(v1 - v2)
    | Operator(Mul) -> InZ(v1 * v2)
    | Operator(Div) -> InZ(v1 / v2)
    | Operator(Eq) -> if v1 = v2 then InZ(1) else InZ(0)
    | Operator(Lt) -> if v1 < v2 then InZ(1) else InZ(0)
    | _ -> failwith "Not defined this binary operation"

(*  *)

and mis_a_jour id_list value_list env =
  match id_list,value_list with
  | [],[] -> env
  | hd_id :: tl_id, hd_v :: tl_v ->
    (hd_id,hd_v) :: ( mis_a_jour tl_id tl_v env) 
  | _ ->  failwith "mis_a_jour id_list value_list not match"


and sem_exprs es env=
  let rec aux es acc env =
  match es with
  | [] -> List.rev acc
  | hd :: tl -> aux tl ((sem_expr hd env) :: acc) env 
  in aux es [] env


and sem_expr expr env = 
  match expr with
  | ASTNum n -> InZ(n)
  | ASTId id -> get_value id env
  | ASTIf (cond,cons,alt) -> 
    let condition = sem_expr cond env in 
      if condition = InZ(1) then sem_expr cons env  
      else if condition = InZ(0) then sem_expr alt env
      else failwith "semantique if error"
  (* | ASTNot b -> if (sem_expr b env) = InZ(1) then InZ(0) else InZ(1) *)
  | ASTAnd (b1,b2) -> let value_b1 = sem_expr b1 env in let value_b2 = sem_expr b2 env in     
    if  value_b1 = InZ(1) then value_b2
    else InZ(0)
  | ASTOr (b1,b2) -> let value_b1 = sem_expr b1 env in let value_b2 = sem_expr b2 env in
    if value_b1 = InZ(1) then InZ(1)
    else value_b2    

  | ASTApp (exprApp,exprs) ->
    (
      let e = sem_expr exprApp env in
      let es = sem_exprs exprs env in
      match e with
      | InZ n -> failwith "App usage : ( expr exprs ) "
      | InF (expr_f,args_f,env_f) -> 
        let env_f_prim = mis_a_jour args_f es env_f in
        sem_expr expr_f env_f_prim
      | InFR (expr_f,name_f,args_f,env_f) -> 
        let env_f_tmp = mis_a_jour args_f es env_f in
        let env_f_prim = (name_f,InFR (expr_f,name_f ,args_f,env_f)) :: env_f_tmp in
        sem_expr expr_f env_f_prim
      | Operator _ -> 
        match List.length es with
        | 1 -> sem_op_unary e (List.hd es)
        | 2 -> sem_op_binary e  (sem_InZ (List.hd es)) (sem_InZ (List.hd (List.tl es)))
        | _ -> failwith "neither unary nor binary"
        
    )
  | ASTBra(args,e_prim) -> InF(e_prim,sem_args args,env)


and sem_args args =
  match args with
  | ASTArg (id,typ)-> [id]
  | ASTArgs (arg,args2) ->
    let (id,_) = arg in 
      id :: (sem_args args2)
    

and sem_def def env :env = 
  match def with
  | ASTConst (id,typ,expr) -> env @ ([(id,sem_expr expr env)]:env) 
  | ASTFun (id,typ,args,expr) ->
   (id,InF(expr,sem_args args,env)) ::env
  | ASTRec (id,typ,args,expr) ->
    (id,InFR(expr,id,sem_args args,env))::env


and sem_stat stat env output=
  match stat with
  | ASTEcho expr -> (sem_expr expr env)::output

and sem_cmds cmds env output =
  match cmds with
  | ASTStat stat-> 
    sem_stat stat env output
  | ASTDefCmd (def,cmds_prim)-> 
    let env_prim = sem_def def env in
    sem_cmds cmds_prim env_prim output

and sem_prog prog = 
  print_output (sem_block prog env0 [])
  
and sem_block block env output = sem_cmds block env output

and print_output output =
  List.iter (function
    | InZ(n) -> Printf.printf "%d\n" n
    | _ -> failwith "not a InZ()"
  ) (List.rev output)
  

let _ =
	try
		let fl = open_in Sys.argv.(1) in
		let lexbuf = Lexing.from_channel fl in
		let p = Parser.prog Lexer.token lexbuf in
			(sem_prog p)
	with Lexer.Eof -> exit 0