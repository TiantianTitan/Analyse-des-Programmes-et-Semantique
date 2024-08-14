(* Programme *)    
type prog = block

(* Block *)
and block = cmds
    
(* Suite de commandes *)

and cmds =
  | ASTStat of stat
  | ASTDefCmd of def * cmds
  | ASTStatCmd of stat * cmds

(* Définition *)
and def =
  | ASTConst of string * typ * expr
  | ASTFun of string * typ * args * expr
  | ASTRec of string * typ * args * expr
  | ASTAssign of string * typ
  | ASTProc of string * argsp * block
  | ASTProcRec of string * argsp * block


(* Type *)
and typ=
  | TYPEINT 
  | TYPEBOOL 
  | ASTArrow of types * typ

and types =
  | ASTType of typ
  | ASTMul of typ * types

(* Paramètre formels (fonctions)*)
and arg = string * typ

and args =
  | ASTArg of arg
  | ASTArgs of arg * args  

(* Paramètre formels (procédure)*)
and argp =  
  | ASTVarp0 of string * typ (* NOT SURE *)
  | ASTVarp of string * typ

and argsp =
  | ASTArgp of argp
  | ASTArgsp of argp * argsp

(* Instruction *)
and stat =
    ASTEcho of expr   
  | ASTSet of string * expr
  | ASTIfMaj of expr * block * block
  | ASTWhile of expr * block
  | ASTCall of string * exprsp

(* Paramètre d'appel *)
and exprp =
  | ASTExpr of expr
  | ASTAdr of string



and exprsp = 
  | ASTExprp of exprp
  | ASTExprsp of exprp * exprsp  

(* Expression *)
and expr =
  | ASTNum of int
  | ASTId of string
  | ASTApp of expr * expr list
  | ASTIf of expr * expr * expr
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTBra of args * expr

(* Suite d’expressions *)  
and exprs =
  | ASTExp of expr
  | ASTExps of expr * exprs
  




     
