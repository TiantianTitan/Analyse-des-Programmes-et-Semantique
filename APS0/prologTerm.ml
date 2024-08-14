open Ast
  
let rec print_typ typ =
  match typ with
  | TYPEBOOL -> "bool"
  | TYPEINT  -> "int"
  | ASTArrow (ts,t) -> "arrow(" ^ print_types ts ^ "," ^ print_typ t ^ ")"

and print_types types =
  match types with
  | ASTType t -> "[" ^ print_typ t ^ "]"
  | ASTMul(t,ts) -> "[" ^ print_typ t ^ print_types_tail ts ^ "]"

and print_types_tail types =
  match types with
  | ASTType t -> "," ^ print_typ t
  | ASTMul(t,ts) -> "," ^ print_typ t ^ print_types_tail ts


and print_arg a =
  match a with
  ident,typ -> "(" ^ ident ^ "," ^ print_typ typ ^ ")"

and print_args args =
  match args with
  | ASTArg a ->  "["^print_arg a^"]"
  | ASTArgs(a,ars) -> "[" ^ print_arg a ^ "," ^ print_args_tails ars ^ "]"

and print_args_tails args = 
  match args with
  | ASTArg a ->  print_arg a
  | ASTArgs(a,ars) -> print_arg a ^ "," ^ print_args_tails ars

and print_expr e =
  match e with
      ASTNum n -> "num(" ^ string_of_int n ^ ")" 
    | ASTId x -> "ident(" ^ x ^")"
    | ASTIf(c,s,a) -> "if(" ^ print_expr c ^ "," ^ print_expr s ^"," ^ print_expr a ^ ")"
    | ASTAnd(x1,x2) -> "and(" ^ print_expr x1 ^ "," ^ print_expr x2 ^ ")"
    | ASTOr(x1,x2) -> "or(" ^ print_expr x1 ^ ","^ print_expr x2^")"
    | ASTApp(e, es) -> "app("  ^ print_expr e ^",["^ print_exprs es ^"])"
    | ASTBra(a,e) -> "[" ^ print_args a ^ "]" ^ print_expr e

    
    

    
and print_exprs es =
  match es with
      [] -> ""
    | [e] -> print_expr e
    | e::es ->  print_expr e ^ "," ^ print_exprs es 
      

and print_stat s =
  match s with
    |ASTEcho e -> "echo(" ^ print_expr(e) ^ ")"
    (* | ASTSet(ident,e) -> "set(" ^ ident ^ "," ^ print_expr e ^ ")"
    | ASTIfMaj(e,b1,b2) -> "IF(" ^ print_expr e ^ "," ^ print_block b1 ^ "," ^ print_block b2 ^")"
    | ASTWhile(e,b) -> "while(" ^ print_expr e ^ "," ^ print_block b ^ ")"
         *)
and print_def def =
  match def with
  | ASTConst(ident,typ,e) ->"const(" ^ ident ^ "," ^ print_typ typ ^"," ^ print_expr e ^ ")"
  | ASTFun (ident,typ,a,e) -> "fun("^ ident ^ "," ^ print_typ typ ^ "," ^ print_args a ^ "," ^ print_expr e ^")"
  | ASTRec (ident,typ,a,e) ->  "funrec("^ ident ^ "," ^ print_typ typ ^ "," ^ print_args a ^ "," ^ print_expr e ^")"
  (* | ASTAssign (ident,typ) -> "var(" ^ ident ^ "," ^(print_typ typ )^")"
  | ASTProc(ident,args,block) ->"proc(" ^ident^ "," ^(print_args args)^ "," ^ (print_block block)^")"
  | ASTProcRec(ident,args,block) ->"procrec(" ^ident^ "," ^(print_args args)^ "," ^ (print_block block)^")" *)

  
and print_cmds cs =
  match cs with
    | ASTStat s -> print_stat s
    | ASTDefCmd(d,c) -> print_def d ^ "," ^ print_cmds c
    (* | ASTStatCmd(s,c) -> print_stat s ^"," ^ print_cmds c *)
     

and print_block b = (print_cmds b) 
	
and print_prog p ="prog([" ^ (print_block p) ^  "])"

;;
	
let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      print_string (print_prog p);
      print_string ".\n"
  with Lexer.Eof ->
    exit 0
      