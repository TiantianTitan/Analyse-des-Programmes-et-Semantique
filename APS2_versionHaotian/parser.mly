%{
(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017                          == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: parser.mly                                                  == *)
(* == Analyse syntaxique                                                   == *)
(* ========================================================================== *)

open Ast

%}
  
%token <int> NUM
%token <string> IDENT
%token LPAR RPAR 
%token LBRA RBRA
%token ECHO CONST FUN REC 
%token IF AND OR
%token PLUS MINUS TIMES DIV LT EQ
%token TYPEBOOL TYPEINT TYPEVOID TYPEVEC
%token SEMICOLON COLON COMMA MUL ARROW
%token ADR VARP
%token ALLOC LEN NTH VSET

%token VAR SET
%token PROC CALL
%token IFMAJ
%token WHILE 


%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.cmds> cmds
%type <Ast.cmds> prog

%start prog

%%
prog: block {$1}
;

block: LBRA cmds RBRA { $2 }
;

cmds:
  stat { ASTStat($1) }
| def SEMICOLON cmds { ASTDefCmd($1,$3) } 
| stat SEMICOLON cmds { ASTStatCmd($1,$3) }
;

def:
  CONST IDENT typ expr { ASTConst($2,$3,$4)}
| FUN IDENT typ LBRA args RBRA expr { ASTFun($2,$3,$5,$7) }
| FUN REC IDENT typ LBRA args RBRA expr { ASTRec($3,$4,$6,$8)  }
| VAR IDENT typ { ASTAssign($2,$3)}
| PROC IDENT LBRA argsp RBRA block {ASTProc($2,$4,$6)}
| PROC REC IDENT LBRA argsp RBRA block {ASTProcRec($3,$5,$7)}
;

typ:
  TYPEBOOL {TYPEBOOL}
| TYPEINT {TYPEINT}
| LPAR types ARROW typ RPAR { ASTArrow($2, $4) }
| LPAR TYPEVEC typ RPAR {ASTVec($3)} 
;

types:
  typ { ASTType($1) }
| typ MUL types {ASTMul($1,$3)}
;

args :
  arg { ASTArg($1) }
| arg COMMA args { ASTArgs($1, $3) }
;

arg :
  IDENT COLON typ { $1,$3 }
;

argsp :
  argp { ASTArgp($1)}
  | argp COMMA argsp { ASTArgsp($1,$3) }
;

argp :
  IDENT COLON typ {ASTVarp0($1,$3) }
| VARP IDENT COLON typ {ASTVarp($2,$4) } 
;

stat:
  ECHO expr { ASTEcho($2) }
| SET lval expr { ASTSet($2,$3) } 
| IFMAJ expr block block {ASTIfMaj($2,$3,$4) }
| WHILE expr block { ASTWhile($2,$3) }
| CALL IDENT exprsp { ASTCall ($2,$3) }
;


lval:
  IDENT {LvalId($1)}
  | LPAR NTH lval expr RPAR {ASTLval($3,$4)}


exprp :
 expr { ASTExpr($1) }
 | LPAR ADR IDENT RPAR { ASTAdr($3) }
;

exprsp :
  exprp {ASTExprp($1)}
 |exprp exprsp {ASTExprsp($1,$2)}
;

expr:
  NUM { ASTNum($1) }
| IDENT { ASTId($1) }
| LPAR IF expr expr expr RPAR { ASTIf($3, $4, $5) }
| LPAR AND expr expr RPAR { ASTAnd($3, $4) }
| LPAR OR expr expr RPAR { ASTOr($3, $4) }
| LPAR expr exprs RPAR  { ASTApp($2, $3) }
| LBRA args RBRA expr { ASTBra($2, $4) }
| LPAR ALLOC expr RPAR { ASTAlloc($3) }
| LPAR LEN expr RPAR { ASTLen($3) }
| LPAR NTH expr expr RPAR { ASTNth($3,$4) }
| LPAR VSET expr expr expr RPAR { ASTVSet($3,$4,$5) }
;

exprs :
  expr { [$1] }
| expr exprs { $1::$2 }
;

