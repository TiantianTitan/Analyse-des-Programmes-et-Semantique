%{
open Ast

%}
  
%token <int> NUM
%token <string> IDENT
%token LPAR RPAR 
%token LBRA RBRA
%token ECHO CONST FUN REC 
%token IF AND OR
%token PLUS MINUS TIMES DIV LT EQ
%token TYPEBOOL TYPEINT
%token SEMICOLON COLON COMMA MUL ARROW


// %token VAR SET
// %token PROC CALL
// %token IFMAJ
// %token WHILE 


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
// | stat SEMICOLON cmds { ASTStatCmd($1,$3) }
;

def:
  CONST IDENT typ expr { ASTConst($2,$3,$4)}
| FUN IDENT typ LBRA args RBRA expr { ASTFun($2,$3,$5,$7) }
| FUN REC IDENT typ LBRA args RBRA expr { ASTRec($3,$4,$6,$8)  }
// | VAR IDENT typ { ASTAssign($2,$3)}
// | PROC IDENT LBRA args RBRA block {ASTProc($2,$4,$6)}
// | PROC REC IDENT LBRA args RBRA block {ASTProcRec($3,$5,$7)}
;

typ:
  TYPEBOOL {TYPEBOOL}
| TYPEINT {TYPEINT}
| LPAR types ARROW typ RPAR { ASTArrow($2, $4) }
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

stat:
  ECHO expr { ASTEcho($2) }
// | SET IDENT expr { ASTSet($2,$3) }
// | IFMAJ expr block block {ASTIfMaj($2,$3,$4) }
// | WHILE expr block { ASTWhile($2,$3) }
// | CALL IDENT exprs { ASTCall ($2,$3) }
;

expr:
  NUM { ASTNum($1) }
| IDENT { ASTId($1) }
| LPAR IF expr expr expr RPAR { ASTIf($3, $4, $5) }
| LPAR AND expr expr RPAR { ASTAnd($3, $4) }
| LPAR OR expr expr RPAR { ASTOr($3, $4) }
| LPAR expr exprs RPAR  { ASTApp($2, $3) }
| LBRA args RBRA expr { ASTBra($2, $4) }
;

exprs :
  expr { [$1] }
| expr exprs { $1::$2 }
;
