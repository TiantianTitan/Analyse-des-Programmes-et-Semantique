open Ast

type value = Value of int (* Value est équilant à InZ *)
| InF of closure | InFR of recClosure
and closure = expr * value list * env (*  update string *)
and recClosure = expr * value * value list * env
and env = (string * value) list  


let rec int_of_value v =
  match v with
  | Value(i) ->  i
  | _ -> failwith "the value is not type of Value()"

and get_value id env =
  match env with
  | [] -> failwith "id is not found in env"
  | hd :: tl -> let (var,value) = hd in
                if var = id then value
                else get_value id tl


(* value *)
and sem_value v = 
  match v with
  | Value i -> i
  | _ -> failwith "Not a value"

(* opétation *)
and sem_op_binary op v1 v2 = 
  match op with
  | "add" -> Value(v1 + v2)
  | "sub" -> Value(v1 - v2)
  | "mul" -> Value(v1 * v2)
  | "div" -> Value(v1 / v2)
  | "eq" -> if v1 = v2 then Value(1) else Value(0)
  | "lt" -> if v1 < v2 then Value(1) else Value(0)
  | _ -> failwith "Not defined this binary operation"


and expr_list es env=
  let rec aux es acc env =
  match es with
  | [] -> List.rev acc
  | hd :: tl -> aux tl ((sem_expr hd env) :: acc) env 
  in aux es [] env


(* and env_insert (env:env) args =
  
  let rec aux args acc :env =
    
    match args with
      | [] -> (List.rev acc) 
      | hd :: tl -> aux tl ( (hd,(sem_expr hd env)) :: acc) 
    
  in aux args []   *)

(* and appli_list f value_list env =
  match f with
  | InF(e_prim,es,env_prim) -> 



  | InFR(value, clo) -> failwith "todo"
  | _ -> failwith "not a function" *)


and args2valueList args_input env=
  match args_input with
  | ASTArg(ident,_) -> [get_value ident env]
  | ASTArgs(arg,args) -> let (ident,_) = arg in (get_value ident env) :: (args2valueList args env)
  | _ -> failwith "args2valueList: args is not ASTArg list"


(* expression *)
and sem_expr expr env= 
  match expr with 
  | ASTBool b -> if b == true then Value(1) else Value(0) 
  | ASTNum n -> Value(n)
  | ASTId id -> get_value id env
  | ASTIf (cond,cons,alt) -> 
      let condition = sem_expr cond env in 
        if condition = Value(1) then sem_expr cons env  
        else if condition = Value(0) then sem_expr alt env
        else failwith "semantique if error"
  | ASTNot b -> if b == ASTBool true then Value(0) else Value(1)
  | ASTAnd (b1,b2) -> let value_b1 = sem_expr b1 env in let value_b2 = sem_expr b2 env in     
    if  value_b1 = Value(1) then value_b2
    else Value(0)
  | ASTOr (b1,b2) -> let value_b1 = sem_expr b1 env in let value_b2 = sem_expr b2 env in
    if value_b1 = Value(1) then Value(1)
    else value_b2    
  
  | ASTBra(args,e_prim) -> InF(e_prim,args2valueList args env,env)
  (* | ASTApp(expr,exprs) ->  *)
(*   
  | ASTApp(e,es) -> let f = sem_expr e env in let l = expr_list es in
  let InF(e_prim,_,_) = f in
  let env_prim = env_insert env l in sem_expr e_prim env_prim *)

  | _ -> failwith "the expr hasn't defined" 



(* programmation *)
and sem_prog prog = sem_block prog
and sem_block block = print_output (sem_cmds block)
and sem_cmds cmds =
  match cmds with
  | ASTStat(stat) -> failwith "todo"
  | ASTDefCmd(def,cmds_prim) -> failwith "todo"




(* commandes
and sem_cmds  = failwith "TODO"
(* arguments *)
and sem_args = failwith "TODO"
(* définition *)
and sem_def = failwith "TODO"
(* states *)
and sem_stat = failwith "TODO"


 *)

and print_val value =
  match value with
    Value(n) -> Printf.printf "%d\n" n
  | _ -> failwith "not a printable value"

and  print_output output =
  List.iter (function x -> print_val x) (List.rev output) 



 let _ =
	try
		let fl = open_in Sys.argv.(1) in
		let lexbuf = Lexing.from_channel fl in
		let p = Parser.prog Lexer.token lexbuf in
			(sem_prog p)
	with Lexer.Eof -> exit 0

