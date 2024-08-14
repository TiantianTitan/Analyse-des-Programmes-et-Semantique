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
| InA of int
| InP of procClosure
| InPR of procRecClosure
| Operator of prim
| Address of int

and closure = expr * string list * env (* update string *)
and recClosure = expr * string * string list * env
and env = (string * value) list  
and espace = None | Some of value | Any
and mem = (int * espace) list           
and procClosure =  block * string list * env
and procRecClosure = block * string * string list * env

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
  | Address(i) -> i
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


and mis_a_jour id_list value_list env =
  match id_list,value_list with
  | [],[] -> env
  | hd_id :: tl_id, hd_v :: tl_v ->
    (hd_id,hd_v) :: ( mis_a_jour tl_id tl_v env) 
  | _ ->  failwith "mis_a_jour id_list value_list not match"


and getInMem mem adr = 
  match mem with
  [] -> failwith "Can't find address in Memory"
  | (v,k):: t when k = adr -> v
  | _ :: t -> getInMem t adr


and newAddress mem = 
  let rec aux memory address = 
    match memory with
    [] -> Address(address+1)
    | (_,k) :: t when k > address -> aux t k
    | (_,k) :: t -> aux t address
  in aux mem 0


and allocMemory mem =
  let adr = newAddress mem in
    match adr with
      Address a -> (adr, mem@[(Any,a)])
      | _ -> failwith "Alloc Failed"


and replaceElem mem oldElem newElem = (
  let rec aux mem oldElem newElem lr = 
    match mem with
    [] -> lr
    | h::t when h = oldElem -> aux t oldElem newElem lr@[newElem]
    | h::t -> aux t oldElem newElem lr@[h]
  in aux mem oldElem newElem []
)


and editInMem mem id adr = 
  let rec aux m id adr = 
    match m with 
    [] -> failwith "Not in Memory"
    | (e,k)::t when k = adr -> replaceElem mem (e,k) (id,k)
    | _::t -> aux t id adr
  in aux mem id adr

and sem_exprs es env mem=
  let rec aux es acc env mem =
  match es with
  | [] -> List.rev acc
  | hd :: tl -> aux tl ((sem_expr hd env mem) :: acc) env mem
  in aux es [] env mem


and sem_expr expr env mem= 
  match expr with
  | ASTNum n -> InZ(n)
  | ASTId id -> (match get_value id env with
                Address a -> (match getInMem mem a with
                              Some v -> v
                              |Any -> InZ 0
                              | _ -> failwith "Cant find id in Mem")
                |v -> v
                ) 
  | ASTIf (cond,cons,alt) -> 
    let condition = sem_expr cond env mem in 
      if condition = InZ(1) then sem_expr cons env mem  
      else if condition = InZ(0) then sem_expr alt env mem
      else failwith "semantique if error"
  (* | ASTNot b -> if (sem_expr b env) = InZ(1) then InZ(0) else InZ(1) *)
  | ASTAnd (b1,b2) -> let value_b1 = sem_expr b1 env mem in let value_b2 = sem_expr b2 env mem in     
    if  value_b1 = InZ(1) then value_b2
    else InZ(0)
  | ASTOr (b1,b2) -> let value_b1 = sem_expr b1 env mem in let value_b2 = sem_expr b2 env mem in
    if value_b1 = InZ(1) then InZ(1)
    else value_b2    

  | ASTApp (exprApp,exprs) ->
    (
      let e = sem_expr exprApp env mem in
      let es = sem_exprs exprs env mem in
      match e with
      | InA a -> failwith "TODO_APPINA"
      | Address a -> failwith "TODO_APP_ADDRESS"
      | InZ n -> failwith "App usage : ( expr exprs ) "
      | InF (expr_f,args_f,env_f) -> 
          let env_f_prim = mis_a_jour args_f es env_f in
            sem_expr expr_f env_f_prim mem
      | InFR (expr_f,name_f,args_f,env_f) -> 
          let env_f_tmp = mis_a_jour args_f es env_f in
            let env_f_prim = (name_f,InFR (expr_f,name_f ,args_f,env_f)) :: env_f_tmp in
              sem_expr expr_f env_f_prim mem
      | Operator _ -> (
        match List.length es with
        | 1 -> sem_op_unary e (List.hd es)
        | 2 -> sem_op_binary e  (sem_InZ (List.hd es)) (sem_InZ (List.hd (List.tl es)))
        | _ -> failwith "neither unary nor binary")
      | _ -> failwith "nothing"
    )
  | ASTBra(args,e_prim) -> InF(e_prim,sem_args args,env)

and sem_exprp exprp env mem = 
match exprp with
    ASTExpr expr -> sem_expr expr env mem
    |ASTAdr adr -> (
      match get_value adr env with
        Address a -> Address a
        |v -> v
    )

and sem_exprsp exprsp env mem = 
match exprsp with
| ASTExprp e -> [sem_exprp e env mem]
| ASTExprsp (e,es) ->  sem_exprp e env mem :: (sem_exprsp es env mem)

and sem_args args =
  match args with
  | ASTArg (id,typ)-> [id]
  | ASTArgs (arg,args2) ->
    let (id,_) = arg in 
      id :: (sem_args args2)

and sem_argp argp =       
  match argp with
  ASTVarp0 (id,typ) -> [id]
  |ASTVarp (id,typ) -> [id] 

and sem_argsp argsp =
  match argsp with
  | ASTArgp argp-> sem_argp argp
  | ASTArgsp (argp,argsp2) ->
  match sem_argp argp with
  | [id] -> id :: (sem_argsp argsp2)
  | _ -> failwith "nothing"


    

and sem_def def env mem = 
  match def with
  | ASTConst (id,typ,expr) -> 
    (env @([(id,sem_expr expr env mem)]) ,mem)
    
  | ASTFun (id,typ,args,expr) ->
   ((id,InF(expr,sem_args args,env)) ::env,mem)

  | ASTRec (id,typ,args,expr) ->
    ((id,InFR(expr,id,sem_args args,env))::env,mem)

  | ASTAssign(id,_) -> let (adr, memRes) = allocMemory mem in ((id, adr)::env, memRes)

  | ASTProc(id,argsp,blk) -> ((id, InP(blk, sem_argsp argsp, env ))::env, mem)

  | ASTProcRec(id, argsp, blk) -> ((id, InPR(blk, id, sem_argsp argsp, env))::env, mem)



and sem_stat stat env mem flux=
  match stat with
  | ASTEcho expr -> 
    (mem, (sem_InZ (sem_expr expr env mem))::flux)

  | ASTSet(id,expr)->( match (get_value id env) with
                    Address a -> let mem' = editInMem mem (Some (sem_expr expr env mem)) a in (mem',flux)
                    |_ -> failwith (id^" not a Var"))

  | ASTIfMaj(cond,cons,alt) -> 
    let condition = sem_expr cond env mem in 
      if condition = InZ(1) then let (mem, flux) = sem_block cons env mem flux  in ((mem, flux)) 
      else if condition = InZ(0) then let (mem, flux) = sem_block alt env mem flux in ((mem, flux))
      else failwith "semantique IF error"

  | ASTWhile(cond,blk) -> 
    let condition = sem_expr cond env mem in 
      if condition = InZ(1) then 
        let (mem, flux) = sem_block blk env mem flux in 
          sem_stat stat env mem flux
      else if condition = InZ(0) then (mem, flux)
      else failwith "semantique While error"

  | ASTCall(id,expr) -> 
    let v = get_value id env in
    let args2 =  sem_exprsp expr env mem  in
      match v with
        InP (blk, args, env_prim) -> 
          let new_env = mis_a_jour args args2 env_prim in
          let (mem_prim, flux_prim) = sem_block blk new_env mem flux in (mem_prim, flux_prim)
        | InPR (blk, id, args, env_closure) ->
          let self_reference_env = (id, InPR (blk, id, args, env_closure)) :: env_closure in
          let new_env = mis_a_jour args args2 self_reference_env in
          let (mem_prim, flux_prim) = sem_block blk new_env mem flux in (mem_prim, flux_prim)
        |_ -> failwith "semantique CALL error"


and sem_cmds cmds env mem flux=
  match cmds with
  | ASTStat stat-> 
    let (mem_prim,flux_prim) = sem_stat stat env mem flux in (env,mem_prim,flux_prim)

  | ASTDefCmd (def,cmds_prim)-> 
    let (env_prim,mem_prim) = sem_def def env mem in sem_cmds cmds_prim env_prim mem_prim flux

  | ASTStatCmd(s,p) -> 
    let (mem_prim,flux_prim) = sem_stat s env mem flux in sem_cmds p env mem_prim flux_prim

and sem_prog (prog:prog) = 
  let (_,flux) = sem_block prog env0 [] []
in  List.rev flux
  
and sem_block block env mem flux =
  let (env_final, mem_final, flux_final) = sem_cmds block env mem flux in
  (mem_final, flux_final)


and print_output output =
  match output with
  |  [] -> ()
  | h::t -> print_int h; print_string "\n";print_output t
  
  

let _ =
	try
		let fl = open_in Sys.argv.(1) in
		let lexbuf = Lexing.from_channel fl in
		let p = Parser.prog Lexer.token lexbuf in
    print_output (sem_prog p)
	with Lexer.Eof -> exit 0
