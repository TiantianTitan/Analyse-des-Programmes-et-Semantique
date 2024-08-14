open Ast

type prim = 
    Add
  | Sub 
  | Eq
  | Lt
  | Mul
  | Div
  | Not

type value = Value of int | Closure of closure | RecClosure of recClosure
            | ProcClosure of procClosure | RecProcClosure of recProcClosure
            | Address of int | Block of int * int
and closure = expr * (value list -> env)
and recClosure = (value -> closure)
and procClosure = (cmds * (value list -> env))
and recProcClosure = value -> procClosure
and flux = int list
and env = (string * value) list

and espace = None | Some of value | Any
and mem = (int * espace) list

exception ArrayOutOfBoundExceptionAPS

let rec int_of_value v =
  match v with
    Value i -> i
    | _ -> 0

let rec int_of_address adr =
      match adr with
        Address a -> a
        | _ -> failwith "Not an Address"

and getInEnv env e =
  match env with
    [] -> failwith "Not in env"
    |(s,v)::t when e = s -> v
    |_::t -> getInEnv t e

and getInMem mem adr =
  match mem with
      [] -> failwith "Not in mem"
      |(k,v)::t when k = adr -> (match v with
                                  Some value -> value
                                  |Any -> Value 0
                                  |_ -> failwith "Not in mem")
      |_::t -> getInMem t adr

and newAddress mem =(
  let rec aux m a =
    match m with
      [] -> Address (a+1)
      |(k,_)::t when k > a -> aux t k
      |(k,_)::t  -> aux t a
  in aux mem 0)

and alloc_memory mem =
  let adr = newAddress mem in
      match adr with
        Address a -> (adr, mem@[(a,Any)])
        |_ -> failwith "Error in allocation"

and alloc_block mem n =
    let adr = newAddress mem in
      match adr with
        Address a -> (let rec aux mem n a' adr' =
                        match a' with
                         adr'' when adr'' == adr'+n -> (adr', mem@[(adr'',Any)])
                        |adr'' -> aux (mem@[(adr'',Any)]) n (adr''+1) adr'
                      in aux mem n a a
        )
        |_ -> failwith "Error in block allocation"

and replaceElem l x e =(
    let rec aux l x e lr = 
      match l with
        [] -> lr
        |h::t when h = x -> aux t x e lr@[e]
        |h::t ->aux t x e lr@[h]
    in aux l x e [])

and editInMem mem adr v =
  let rec aux m adr v =
    match m with
      [] -> failwith "Not in mem"
      |(k,e)::t when k = adr -> replaceElem mem (k,e) (k,v)
      |_::t -> aux t adr v
    in aux mem adr v

and eval_prog p =
  let (_, flux) = eval_block ([], [], []) p in
    List.rev flux

and eval_block etatInit b =
  let (_,mem,flux) = List.fold_left eval_cmd etatInit b in
  (mem, flux)

and eval_cmd (env,mem,flux) cmd =
  match cmd with
    ASTStat stat -> let (mem, flux) = eval_stat (env,mem,flux) stat in (env,mem,flux)
  | ASTDefCmd (def,cmds_prim)-> 
    let (env_prim,mem_prim) = eval_def def env mem in sem_cmds cmds_prim env_prim mem_prim flux

  | ASTStatCmd(s,p) -> 
    let (mem_prim,flux_prim) = sem_stat s env mem flux in sem_cmds p env mem_prim flux_prim

and eval_args env args =
  fun a -> (List.combine (List.map get_id_arg args) a)@env

and get_id_arg arg =
      match arg with
        Arg (id,_) -> id
        |Argp (id,_) -> id

and eval_def (env, mem) def =
  match def with
    ConstDef (id, _, e) -> let e',mem' = eval_sexpr (env, mem) e in ((id, e')::env,mem')
    | FunDef (id, _, args, e) -> ((id,Closure(e,eval_args env args))::env,mem)
    | RecFunDef (id, _, args, e) -> ((id, RecClosure(fun f -> (e, fun a -> (id,f)::(List.combine (List.map get_id_arg args) a)@env)))::env,mem)
    | VarDef (id,_) -> let (a, memRes) = alloc_memory mem in ((id, a)::env, memRes)
    | ProcDef (id, args, b) -> ((id, ProcClosure(b, eval_args env args))::env, mem)
    | RecProcDef (id, args, b) -> ((id, RecProcClosure(fun p -> (b, fun a -> (id,p)::(List.combine (List.map get_id_arg args) a)@env)))::env,mem)

and eval_stat (env,mem,flux) stat =
  match stat with
  |Echo e -> ( match (eval_sexpr (env, mem) e) with
                    (Value i,mem') -> (mem',(i::flux))
                    | _ -> failwith "Error ECHO" )
  |Set (id, e) -> ( let a = eval_lvalue (env,mem) id and (v,mem') = eval_sexpr (env,mem) e in match a with
                    adr -> let mem' = editInMem mem' adr (Some v) in (mem',flux)
                   )
  |IfStat (c, i, e) -> (match (eval_sexpr (env, mem) c) with
                        (Value 1,mem') -> let (mem',flux) = eval_block (env, mem', flux) i in
                                    (mem', flux)
                        |(Value 0,mem') -> let (mem',flux) = eval_block (env, mem', flux) e in
                        (mem', flux)
                        |_ -> failwith "Error in if condition")
  |While (c,b) -> (match (eval_sexpr (env, mem) c) with
                    (Value 0,mem') -> (mem',flux)
                    |(Value 1,mem') -> let (mem',f) = eval_block (env, mem',flux) b in
                                eval_stat (env, mem',f) (While (c,b))
                    |_ -> failwith "Error in while condition")
                    |Call (id,e) -> let p = getInEnv env id in
                    let (args, mem') = (List.fold_left(fun (a, m) e -> let (v, m') = eval_sexprp (env, m) e in v::a, m') ([], mem) e) in
                    (match p with
                      ProcClosure (b, r) -> let (mem'', flux') = eval_block ((r (List.rev args)), mem', flux) b in
                        (mem'', flux')
                      | RecProcClosure rp -> let (b, r) = rp p in
                        let (mem'', flux') = eval_block ((r (List.rev args)), mem', flux) b in
                        (mem'', flux')
                      | _ -> failwith "Error in Call")

and eval_lvalue (env,mem) lval =
    match lval with
      LvalueId x -> (match getInEnv env x with
                        |Address a -> a
                        |Block (a,n) -> (a+1)
                        |_ -> failwith "Not an Lvalue"
      )
      |Lvalue (lval, e) -> (match eval_lvalue (env,mem) lval with
                              a -> (match eval_sexpr (env,mem) e with
                                            (Value i,mem') -> (match getInMem mem' (a+i) with
                                                              Value n -> (a+i)
                                                              |Block(a',n) -> (a'+i)
                                                              |_ ->failwith "Error in lnth")
                                            |_ -> failwith "Error in lnth")
                              )

and eval_sexprp (env, mem) value =
  match value with
    ASTAdr x -> (match getInEnv env x with
                  Address a -> (Address a,mem)
                  | v -> (v,mem))
  | ASTExpr e -> eval_sexpr (env, mem) e

and print_mem mem =
    match mem with
      [] -> ()
      |(k,Some v)::t -> print_int k; print_string " "; print_int(int_of_value v); print_string "\n";print_mem t
      |(k,Any)::t -> print_int k; print_string " Any\n";print_mem t
      |(_,_)::t -> print_mem t
and print_env env =
    match env with
      [] -> ()
      |(id,v)::t -> print_string id; print_string " "; print_int(int_of_value v); print_string "\n";print_env t

and eval_sexpr (env, mem) value =
  match value with
    ASTBool true -> (Value 1,mem)
    | ASTBool false -> (Value 0,mem)
    | ASTNum n -> (Value n,mem)
    | ASTId x -> (match getInEnv env x with
                  Address a -> (getInMem mem a,mem)
                  | v -> (v,mem))
    | ASTIf (c, i, e) ->  (match (eval_sexpr (env, mem) c) with
                          (Value 1,mem') -> eval_sexpr (env, mem') i
                          |(Value 0,mem') -> eval_sexpr (env, mem') e
                          |_ -> failwith "Error in if condition")
    | ASTOp (op,vals) -> eval_op (env,mem) (op,vals)
    | ASTOp (op,vals) -> eval_op (env,mem) (op,vals)
    | ASTApp(f, es) -> let (c,mem') = eval_sexpr (env, mem) f in
                      let (args, mem'') = (List.fold_left (fun (a, m) e -> let (v, m') = eval_sexpr (env, m) e in v::a, m') ([], mem') es) in
                            (match c with
                              Closure (e', r) -> eval_sexpr ((r (List.rev args)),mem'') e'
                            | RecClosure rf -> let (e', r) = (rf c) in eval_sexpr ((r (List.rev args)),mem'') e'
                            | _ -> failwith "Error in App")
    | ASTFunAbs(args, e) -> (Closure (e, eval_args env args),mem)

    and eval_op (env,mem) (op,vals) = 
    match (op,vals) with
      ("not",[val1]) -> (match (eval_sexpr (env,mem) val1) with
                      (Value 1,mem') -> (Value 0,mem')
                      |(Value 0,mem') -> (Value 1,mem')
                      |_ -> failwith "Error in NOT argument")
      | ("and",[val1;val2]) -> let (val1',mem') = eval_sexpr (env,mem) val1 in let (val2',mem') = eval_sexpr (env,mem') val2 in
                              (match ((int_of_value val1'),(int_of_value val2')) with
                                (1,1) -> (Value 1,mem')
                                |(_,_) -> (Value 0,mem')
                              )
      | ("or",[val1;val2]) -> let (val1',mem') = eval_sexpr (env,mem) val1 in let (val2',mem') = eval_sexpr (env,mem') val2 in
                              (match ((int_of_value val1'),(int_of_value val2')) with
                                (0,0) -> (Value 0,mem')
                                |(_,_) -> (Value 1,mem')
                              )
      | ("eq",[val1;val2]) -> let (val1',mem') = eval_sexpr (env,mem) val1 in let (val2',mem') = eval_sexpr (env,mem') val2 in
                              (match (int_of_value val1') == (int_of_value val2') with
                                true -> (Value 1,mem')
                                |false -> (Value 0,mem')
                              )
      | ("lt",[val1;val2])  -> let (val1',mem') = eval_sexpr (env,mem) val1 in let (val2',mem') = eval_sexpr (env,mem') val2 in
                              (match (int_of_value val1') < (int_of_value val2') with
                                true -> (Value 1,mem')
                                |false -> (Value 0,mem')
                              )
      | ("add",[val1;val2])  -> let (val1',mem') = eval_sexpr (env,mem) val1 in let (val2',mem') = eval_sexpr (env,mem') val2
                                in (Value ((int_of_value val1') + (int_of_value val2')),mem')
      | ("sub",[val1;val2])  -> let (val1',mem') = eval_sexpr (env,mem) val1 in let (val2',mem') = eval_sexpr (env,mem') val2
                                in (Value ((int_of_value val1') - (int_of_value val2')),mem')
      | ("mul",[val1;val2])  -> let (val1',mem') = eval_sexpr (env,mem) val1 in let (val2',mem') = eval_sexpr (env,mem') val2
                                in (Value ((int_of_value val1') * (int_of_value val2')),mem')
      | ("div",[val1;val2])  -> let (val1',mem') = eval_sexpr (env,mem) val1 in let (val2',mem') = eval_sexpr (env,mem') val2
                                in (Value ((int_of_value val1') / (int_of_value val2')),mem')
      | ("alloc",[val1]) -> (match eval_sexpr (env, mem) val1 with
                             (Value n,mem') -> let (a,mem') = alloc_block mem n in
                                                  (Block (a,n), mem')
                                                  |_ -> failwith "Error in ALLOC"
      )
      | ("len",[val1]) -> (match eval_sexpr (env, mem) val1 with
                              (Block(_, n),mem') -> (Value n, mem')
                              |_ -> failwith "Error in LEN"
                          )
      | ("nth",[val1;val2]) -> let(val1', mem') = eval_sexpr (env, mem) val1 in
                              let (val2', mem') = eval_sexpr (env, mem') val2 in
                              (match val1', val2' with
                               (Block (adr, n), Value v) -> (getInMem mem' (adr+v+1),mem')
                               |_ -> failwith "Error in NTH, not in memory")
      |_ -> failwith "Error in OP arguments"
;;

let rec print_list l =
        match l with
        [] -> ()
        |h::t -> print_int h; print_string "\n";print_list t

;;

let fname = Sys.argv.(1) in
let ic = open_in fname in
      try
        let lexbuf = Lexing.from_channel ic in
        let e = Parser.prog Lexer.token lexbuf in
        print_list(eval_prog e);
      with Lexer.Eof ->
        exit 0