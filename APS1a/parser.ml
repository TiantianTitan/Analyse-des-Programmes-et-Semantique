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
  | TYPEVOID
  | SEMICOLON
  | COLON
  | COMMA
  | MUL
  | ARROW
  | ADR
  | VARP
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

# 54 "parser.ml"
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
  278 (* TYPEVOID *);
  279 (* SEMICOLON *);
  280 (* COLON *);
  281 (* COMMA *);
  282 (* MUL *);
  283 (* ARROW *);
  284 (* ADR *);
  285 (* VARP *);
  286 (* VAR *);
  287 (* SET *);
  288 (* PROC *);
  289 (* CALL *);
  290 (* IFMAJ *);
  291 (* WHILE *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\004\000\005\000\003\000\003\000\003\000\007\000\007\000\007\000\
\007\000\007\000\007\000\008\000\008\000\008\000\011\000\011\000\
\009\000\009\000\012\000\010\000\010\000\013\000\013\000\006\000\
\006\000\006\000\006\000\006\000\015\000\015\000\014\000\014\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000\
\002\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\004\000\007\000\008\000\
\003\000\006\000\007\000\001\000\001\000\005\000\001\000\003\000\
\001\000\003\000\003\000\001\000\003\000\003\000\004\000\002\000\
\003\000\004\000\003\000\003\000\001\000\004\000\001\000\002\000\
\001\000\001\000\006\000\005\000\005\000\004\000\004\000\001\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\042\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\033\000\034\000\000\000\000\000\024\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\012\000\013\000\000\000\000\000\000\000\
\009\000\025\000\000\000\000\000\000\000\029\000\028\000\000\000\
\000\000\027\000\005\000\004\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\006\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\032\000\
\026\000\000\000\000\000\000\000\041\000\038\000\019\000\039\000\
\018\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\036\000\037\000\016\000\000\000\
\000\000\000\000\022\000\000\000\010\000\021\000\000\000\030\000\
\035\000\014\000\007\000\000\000\023\000\011\000\008\000"

let yydgoto = "\002\000\
\039\000\065\000\015\000\004\000\005\000\016\000\017\000\069\000\
\041\000\076\000\070\000\042\000\077\000\055\000\056\000"

let yysindex = "\004\000\
\009\255\000\000\070\255\000\000\000\000\065\255\014\255\010\255\
\027\255\033\255\015\255\041\255\065\255\065\255\021\255\028\255\
\029\255\000\000\000\000\080\255\051\255\000\000\025\255\025\255\
\052\255\025\255\065\255\054\255\058\255\093\255\009\255\009\255\
\000\000\070\255\070\255\065\255\065\255\065\255\065\255\038\255\
\057\255\040\255\025\255\000\000\000\000\065\255\066\255\025\255\
\000\000\000\000\013\255\068\255\036\255\000\000\000\000\093\255\
\009\255\000\000\000\000\000\000\065\255\065\255\065\255\065\255\
\076\255\025\255\065\255\051\255\048\255\060\255\000\000\051\255\
\079\255\064\255\087\255\091\255\081\255\013\255\105\255\000\000\
\000\000\065\255\104\255\107\255\000\000\000\000\000\000\000\000\
\000\000\025\255\025\255\103\255\051\255\025\255\088\255\009\255\
\013\255\108\255\109\255\111\255\000\000\000\000\000\000\112\255\
\065\255\113\255\000\000\025\255\000\000\000\000\009\255\000\000\
\000\000\000\000\000\000\065\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\114\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\115\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\118\255\
\000\000\000\000\000\000\000\000\090\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\117\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\250\255\054\000\240\255\000\000\235\255\000\000\000\000\234\255\
\197\255\181\255\034\000\000\000\000\000\069\000\000\000"

let yytablesize = 125
let yytable = "\022\000\
\046\000\047\000\098\000\049\000\001\000\031\000\031\000\032\000\
\089\000\057\000\058\000\024\000\092\000\003\000\074\000\023\000\
\028\000\059\000\060\000\025\000\050\000\110\000\031\000\054\000\
\029\000\073\000\033\000\043\000\026\000\061\000\062\000\063\000\
\064\000\106\000\027\000\081\000\018\000\019\000\020\000\071\000\
\021\000\075\000\030\000\087\000\044\000\045\000\036\000\037\000\
\038\000\054\000\034\000\035\000\040\000\048\000\082\000\083\000\
\084\000\064\000\051\000\052\000\088\000\066\000\067\000\079\000\
\068\000\018\000\019\000\020\000\104\000\021\000\072\000\107\000\
\078\000\090\000\109\000\100\000\006\000\007\000\008\000\086\000\
\018\000\019\000\020\000\093\000\021\000\117\000\091\000\094\000\
\095\000\118\000\036\000\037\000\038\000\018\000\019\000\053\000\
\096\000\021\000\115\000\009\000\010\000\011\000\012\000\013\000\
\014\000\097\000\099\000\101\000\105\000\119\000\102\000\108\000\
\112\000\111\000\113\000\114\000\015\000\085\000\116\000\003\000\
\017\000\040\000\020\000\103\000\080\000"

let yycheck = "\006\000\
\023\000\024\000\078\000\026\000\001\000\006\001\013\000\014\000\
\068\000\031\000\032\000\002\001\072\000\005\001\002\001\002\001\
\002\001\034\000\035\000\010\001\027\000\097\000\023\001\030\000\
\010\001\048\000\006\001\003\001\002\001\036\000\037\000\038\000\
\039\000\093\000\002\001\057\000\001\001\002\001\003\001\046\000\
\005\001\029\001\002\001\066\000\020\001\021\001\011\001\012\001\
\013\001\056\000\023\001\023\001\002\001\002\001\061\000\062\000\
\063\000\064\000\005\001\002\001\067\000\024\001\006\001\028\001\
\025\001\001\001\002\001\003\001\091\000\005\001\005\001\094\000\
\005\001\026\001\096\000\082\000\007\001\008\001\009\001\004\001\
\001\001\002\001\003\001\005\001\005\001\108\000\027\001\024\001\
\002\001\111\000\011\001\012\001\013\001\001\001\002\001\003\001\
\006\001\005\001\105\000\030\001\031\001\032\001\033\001\034\001\
\035\001\025\001\002\001\004\001\006\001\116\000\004\001\024\001\
\004\001\006\001\004\001\004\001\027\001\064\000\006\001\006\001\
\006\001\004\001\006\001\090\000\056\000"

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
  TYPEVOID\000\
  SEMICOLON\000\
  COLON\000\
  COMMA\000\
  MUL\000\
  ARROW\000\
  ADR\000\
  VARP\000\
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
# 42 "parser.mly"
            (_1)
# 256 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 45 "parser.mly"
                      ( _2 )
# 263 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 49 "parser.mly"
       ( ASTStat(_1) )
# 270 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 50 "parser.mly"
                     ( ASTDefCmd(_1,_3) )
# 278 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 51 "parser.mly"
                      ( ASTStatCmd(_1,_3) )
# 286 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 55 "parser.mly"
                       ( ASTConst(_2,_3,_4))
# 295 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 56 "parser.mly"
                                    ( ASTFun(_2,_3,_5,_7) )
# 305 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 57 "parser.mly"
                                        ( ASTRec(_3,_4,_6,_8)  )
# 315 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 58 "parser.mly"
                ( ASTAssign(_2,_3))
# 323 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'argsp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 59 "parser.mly"
                                   (ASTProc(_2,_4,_6))
# 332 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'argsp) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 60 "parser.mly"
                                       (ASTProcRec(_3,_5,_7))
# 341 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
           (TYPEBOOL)
# 347 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
          (TYPEINT)
# 353 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 66 "parser.mly"
                            ( ASTArrow(_2, _4) )
# 361 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 70 "parser.mly"
      ( ASTType(_1) )
# 368 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 71 "parser.mly"
                (ASTMul(_1,_3))
# 376 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 75 "parser.mly"
      ( ASTArg(_1) )
# 383 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 76 "parser.mly"
                 ( ASTArgs(_1, _3) )
# 391 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 80 "parser.mly"
                  ( _1,_3 )
# 399 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'argp) in
    Obj.repr(
# 84 "parser.mly"
       ( ASTArgp(_1))
# 406 "parser.ml"
               : 'argsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'argp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'argsp) in
    Obj.repr(
# 85 "parser.mly"
                     ( ASTArgsp(_1,_3) )
# 414 "parser.ml"
               : 'argsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 89 "parser.mly"
                  (ASTVarp0(_1,_3) )
# 422 "parser.ml"
               : 'argp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 90 "parser.mly"
                       (ASTVarp(_2,_4) )
# 430 "parser.ml"
               : 'argp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 94 "parser.mly"
            ( ASTEcho(_2) )
# 437 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 95 "parser.mly"
                 ( ASTSet(_2,_3) )
# 445 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 96 "parser.mly"
                         (ASTIfMaj(_2,_3,_4) )
# 454 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 97 "parser.mly"
                   ( ASTWhile(_2,_3) )
# 462 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 98 "parser.mly"
                    ( ASTCall (_2,_3) )
# 470 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 103 "parser.mly"
      ( ASTExpr(_1) )
# 477 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 104 "parser.mly"
                       ( ASTAdr(_3) )
# 484 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exprp) in
    Obj.repr(
# 108 "parser.mly"
        (ASTExprp(_1))
# 491 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exprp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 109 "parser.mly"
               (ASTExprsp(_1,_2))
# 499 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 113 "parser.mly"
      ( ASTNum(_1) )
# 506 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 114 "parser.mly"
        ( ASTId(_1) )
# 513 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 115 "parser.mly"
                              ( ASTIf(_3, _4, _5) )
# 522 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 116 "parser.mly"
                          ( ASTAnd(_3, _4) )
# 530 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 117 "parser.mly"
                         ( ASTOr(_3, _4) )
# 538 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 118 "parser.mly"
                        ( ASTApp(_2, _3) )
# 546 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 119 "parser.mly"
                      ( ASTBra(_2, _4) )
# 554 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 123 "parser.mly"
       ( [_1] )
# 561 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 124 "parser.mly"
             ( _1::_2 )
# 569 "parser.ml"
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
