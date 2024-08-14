type token =
  | NUM of (int)
  | IDENT of (string)
  | LPAR
  | RPAR
  | LBRA
  | RBRA
  | ECHO
  | CONST
  | FUN
  | REC
  | IF
  | AND
  | OR
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LT
  | EQ
  | TYPEBOOL
  | TYPEINT
  | SEMICOLON
  | COLON
  | COMMA
  | MUL
  | ARROW
  | VAR
  | SET
  | PROC
  | CALL
  | IFMAJ
  | WHILE

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
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

# 51 "parser.ml"
let yytransl_const = [|
  259 (* LPAR *);
  260 (* RPAR *);
  261 (* LBRA *);
  262 (* RBRA *);
  263 (* ECHO *);
  264 (* CONST *);
  265 (* FUN *);
  266 (* REC *);
  267 (* IF *);
  268 (* AND *);
  269 (* OR *);
  270 (* PLUS *);
  271 (* MINUS *);
  272 (* TIMES *);
  273 (* DIV *);
  274 (* LT *);
  275 (* EQ *);
  276 (* TYPEBOOL *);
  277 (* TYPEINT *);
  278 (* SEMICOLON *);
  279 (* COLON *);
  280 (* COMMA *);
  281 (* MUL *);
  282 (* ARROW *);
  283 (* VAR *);
  284 (* SET *);
  285 (* PROC *);
  286 (* CALL *);
  287 (* IFMAJ *);
  288 (* WHILE *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\004\000\005\000\003\000\003\000\003\000\007\000\007\000\007\000\
\007\000\007\000\007\000\008\000\008\000\008\000\010\000\010\000\
\009\000\009\000\011\000\006\000\006\000\006\000\006\000\006\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000\
\002\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\004\000\007\000\008\000\
\003\000\006\000\007\000\001\000\001\000\005\000\001\000\003\000\
\001\000\003\000\003\000\002\000\003\000\004\000\003\000\003\000\
\001\000\001\000\006\000\005\000\005\000\004\000\004\000\001\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\034\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\025\000\026\000\000\000\000\000\020\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\012\000\013\000\000\000\000\000\000\000\
\009\000\021\000\000\000\000\000\000\000\024\000\000\000\023\000\
\005\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\006\000\000\000\000\000\000\000\000\000\
\033\000\022\000\000\000\000\000\000\000\030\000\019\000\031\000\
\018\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\028\000\029\000\016\000\000\000\000\000\000\000\010\000\000\000\
\027\000\014\000\007\000\000\000\011\000\008\000"

let yydgoto = "\002\000\
\053\000\054\000\015\000\004\000\005\000\016\000\017\000\066\000\
\041\000\067\000\042\000"

let yysindex = "\012\000\
\017\255\000\000\043\255\000\000\000\000\044\255\033\255\010\255\
\034\255\036\255\027\255\039\255\044\255\044\255\009\255\020\255\
\022\255\000\000\000\000\077\255\046\255\000\000\013\255\013\255\
\055\255\013\255\044\255\054\255\058\255\044\255\017\255\017\255\
\000\000\043\255\043\255\044\255\044\255\044\255\044\255\038\255\
\056\255\040\255\013\255\000\000\000\000\044\255\060\255\013\255\
\000\000\000\000\046\255\062\255\044\255\000\000\017\255\000\000\
\000\000\000\000\044\255\044\255\044\255\064\255\013\255\044\255\
\046\255\051\255\057\255\000\000\046\255\072\255\075\255\046\255\
\000\000\000\000\044\255\080\255\081\255\000\000\000\000\000\000\
\000\000\013\255\013\255\085\255\046\255\017\255\086\255\082\255\
\000\000\000\000\000\000\089\255\044\255\090\255\000\000\017\255\
\000\000\000\000\000\000\044\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\091\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\092\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\005\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\069\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\250\255\227\255\239\255\000\000\226\255\000\000\000\000\236\255\
\210\255\017\000\000\000"

let yytablesize = 99
let yytable = "\022\000\
\055\000\056\000\046\000\047\000\071\000\049\000\031\000\032\000\
\032\000\062\000\032\000\024\000\001\000\039\000\033\000\043\000\
\057\000\058\000\081\000\025\000\050\000\003\000\084\000\073\000\
\074\000\087\000\032\000\070\000\028\000\059\000\060\000\061\000\
\044\000\045\000\023\000\026\000\029\000\027\000\094\000\068\000\
\030\000\034\000\079\000\035\000\018\000\019\000\020\000\040\000\
\021\000\006\000\007\000\008\000\075\000\076\000\077\000\095\000\
\048\000\080\000\051\000\052\000\063\000\064\000\092\000\065\000\
\069\000\101\000\072\000\078\000\088\000\009\000\010\000\011\000\
\012\000\013\000\014\000\082\000\085\000\018\000\019\000\020\000\
\086\000\021\000\083\000\089\000\090\000\097\000\099\000\036\000\
\037\000\038\000\093\000\096\000\098\000\102\000\015\000\100\000\
\003\000\017\000\091\000"

let yycheck = "\006\000\
\031\000\032\000\023\000\024\000\051\000\026\000\013\000\014\000\
\004\001\039\000\006\001\002\001\001\000\020\000\006\001\003\001\
\034\000\035\000\065\000\010\001\027\000\005\001\069\000\053\000\
\055\000\072\000\022\001\048\000\002\001\036\000\037\000\038\000\
\020\001\021\001\002\001\002\001\010\001\002\001\085\000\046\000\
\002\001\022\001\063\000\022\001\001\001\002\001\003\001\002\001\
\005\001\007\001\008\001\009\001\059\000\060\000\061\000\086\000\
\002\001\064\000\005\001\002\001\023\001\006\001\083\000\024\001\
\005\001\096\000\005\001\004\001\075\000\027\001\028\001\029\001\
\030\001\031\001\032\001\025\001\005\001\001\001\002\001\003\001\
\006\001\005\001\026\001\004\001\004\001\004\001\093\000\011\001\
\012\001\013\001\006\001\006\001\004\001\100\000\026\001\006\001\
\006\001\006\001\082\000"

let yynames_const = "\
  LPAR\000\
  RPAR\000\
  LBRA\000\
  RBRA\000\
  ECHO\000\
  CONST\000\
  FUN\000\
  REC\000\
  IF\000\
  AND\000\
  OR\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  LT\000\
  EQ\000\
  TYPEBOOL\000\
  TYPEINT\000\
  SEMICOLON\000\
  COLON\000\
  COMMA\000\
  MUL\000\
  ARROW\000\
  VAR\000\
  SET\000\
  PROC\000\
  CALL\000\
  IFMAJ\000\
  WHILE\000\
  "

let yynames_block = "\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 41 "parser.mly"
            (_1)
# 233 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 44 "parser.mly"
                      ( _2 )
# 240 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 48 "parser.mly"
       ( ASTStat(_1) )
# 247 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 49 "parser.mly"
                     ( ASTDefCmd(_1,_3) )
# 255 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 50 "parser.mly"
                      ( ASTStatCmd(_1,_3) )
# 263 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 54 "parser.mly"
                       ( ASTConst(_2,_3,_4))
# 272 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 55 "parser.mly"
                                    ( ASTFun(_2,_3,_5,_7) )
# 282 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 56 "parser.mly"
                                        ( ASTRec(_3,_4,_6,_8)  )
# 292 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 57 "parser.mly"
                ( ASTAssign(_2,_3))
# 300 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 58 "parser.mly"
                                  (ASTProc(_2,_4,_6))
# 309 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 59 "parser.mly"
                                      (ASTProcRec(_3,_5,_7))
# 318 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
           (TYPEBOOL)
# 324 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
          (TYPEINT)
# 330 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 65 "parser.mly"
                            ( ASTArrow(_2, _4) )
# 338 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 69 "parser.mly"
      ( ASTType(_1) )
# 345 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 70 "parser.mly"
                (ASTMul(_1,_3))
# 353 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 74 "parser.mly"
      ( ASTArg(_1) )
# 360 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 75 "parser.mly"
                 ( ASTArgs(_1, _3) )
# 368 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 79 "parser.mly"
                  ( _1,_3 )
# 376 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 83 "parser.mly"
            ( ASTEcho(_2) )
# 383 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 84 "parser.mly"
                 ( ASTSet(_2,_3) )
# 391 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 85 "parser.mly"
                         (ASTIfMaj(_2,_3,_4) )
# 400 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 86 "parser.mly"
                   ( ASTWhile(_2,_3) )
# 408 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 87 "parser.mly"
                   ( ASTCall (_2,_3) )
# 416 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 91 "parser.mly"
      ( ASTNum(_1) )
# 423 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "parser.mly"
        ( ASTId(_1) )
# 430 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 93 "parser.mly"
                              ( ASTIf(_3, _4, _5) )
# 439 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 94 "parser.mly"
                          ( ASTAnd(_3, _4) )
# 447 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 95 "parser.mly"
                         ( ASTOr(_3, _4) )
# 455 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 96 "parser.mly"
                        ( ASTApp(_2, _3) )
# 463 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 97 "parser.mly"
                      ( ASTBra(_2, _4) )
# 471 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 101 "parser.mly"
       ( [_1] )
# 478 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 102 "parser.mly"
             ( _1::_2 )
# 486 "parser.ml"
               : Ast.expr list))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.cmds)
