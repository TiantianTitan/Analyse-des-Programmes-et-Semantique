(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)

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
  | ASTVec of typ (*APS2*)

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
  | ASTSet of lval * expr (* APS2 *)
  | ASTIfMaj of expr * block * block
  | ASTWhile of expr * block
  | ASTCall of string * exprsp

(* APS2 *)  
and lval =
   LvalId of string
  | ASTLval of lval * expr


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
  (* APS2 *)
  | ASTAlloc of expr
  | ASTLen of expr
  | ASTNth of expr * expr
  | ASTVSet of expr * expr * expr

(* Suite d’expressions *)  
and exprs =
  | ASTExp of expr
  | ASTExps of expr * exprs
  




     
