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
  | TYPEVEC
  | SEMICOLON
  | COLON
  | COMMA
  | MUL
  | ARROW
  | ADR
  | VARP
  | ALLOC
  | LEN
  | NTH
  | VSET
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

# 59 "parser.ml"
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
  279 (* TYPEVEC *);
  280 (* SEMICOLON *);
  281 (* COLON *);
  282 (* COMMA *);
  283 (* MUL *);
  284 (* ARROW *);
  285 (* ADR *);
  286 (* VARP *);
  287 (* ALLOC *);
  288 (* LEN *);
  289 (* NTH *);
  290 (* VSET *);
  291 (* VAR *);
  292 (* SET *);
  293 (* PROC *);
  294 (* CALL *);
  295 (* IFMAJ *);
  296 (* WHILE *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\004\000\005\000\003\000\003\000\003\000\007\000\007\000\007\000\
\007\000\007\000\007\000\008\000\008\000\008\000\008\000\011\000\
\011\000\009\000\009\000\012\000\010\000\010\000\013\000\013\000\
\006\000\006\000\006\000\006\000\006\000\014\000\014\000\016\000\
\016\000\015\000\015\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\002\000\002\000\
\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\004\000\007\000\008\000\
\003\000\006\000\007\000\001\000\001\000\005\000\004\000\001\000\
\003\000\001\000\003\000\003\000\001\000\003\000\003\000\004\000\
\002\000\003\000\004\000\003\000\003\000\001\000\005\000\001\000\
\004\000\001\000\002\000\001\000\001\000\006\000\005\000\005\000\
\004\000\004\000\004\000\004\000\005\000\006\000\001\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\049\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\036\000\037\000\000\000\000\000\025\000\000\000\000\000\
\000\000\000\000\030\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\012\000\013\000\000\000\000\000\000\000\009\000\000\000\
\026\000\000\000\000\000\000\000\032\000\029\000\000\000\000\000\
\028\000\005\000\004\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\035\000\027\000\000\000\000\000\
\000\000\043\000\044\000\000\000\000\000\048\000\041\000\020\000\
\042\000\019\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\039\000\
\040\000\045\000\000\000\015\000\017\000\000\000\000\000\000\000\
\031\000\023\000\000\000\010\000\022\000\000\000\033\000\038\000\
\046\000\014\000\007\000\000\000\024\000\011\000\008\000"

let yydgoto = "\002\000\
\045\000\076\000\015\000\004\000\005\000\016\000\017\000\081\000\
\047\000\089\000\082\000\048\000\090\000\029\000\062\000\063\000"

let yysindex = "\014\000\
\020\255\000\000\013\255\000\000\000\000\053\255\026\255\001\255\
\029\255\002\255\004\255\057\255\053\255\053\255\065\255\055\255\
\058\255\000\000\000\000\124\255\075\255\000\000\040\255\040\255\
\079\255\040\255\000\000\050\255\053\255\080\255\082\255\073\255\
\020\255\020\255\000\000\013\255\013\255\053\255\053\255\053\255\
\053\255\053\255\053\255\053\255\053\255\067\255\087\255\068\255\
\021\255\000\000\000\000\053\255\094\255\040\255\000\000\002\255\
\000\000\255\254\095\255\085\255\000\000\000\000\073\255\020\255\
\000\000\000\000\000\000\053\255\053\255\053\255\097\255\099\255\
\053\255\053\255\053\255\100\255\040\255\053\255\075\255\040\255\
\078\255\081\255\000\000\075\255\102\255\053\255\083\255\109\255\
\106\255\089\255\255\254\111\255\000\000\000\000\053\255\116\255\
\118\255\000\000\000\000\119\255\053\255\000\000\000\000\000\000\
\000\000\000\000\126\255\040\255\040\255\122\255\075\255\127\255\
\040\255\107\255\020\255\255\254\132\255\129\255\135\255\000\000\
\000\000\000\000\136\255\000\000\000\000\137\255\053\255\138\255\
\000\000\000\000\040\255\000\000\000\000\020\255\000\000\000\000\
\000\000\000\000\000\000\053\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\139\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\140\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\006\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\143\255\000\000\000\000\000\000\000\000\000\000\
\114\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\142\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\250\255\068\000\029\000\000\000\232\255\000\000\000\000\249\255\
\190\255\167\255\041\000\000\000\000\000\094\000\088\000\000\000"

let yytablesize = 158
let yytable = "\022\000\
\087\000\117\000\024\000\027\000\028\000\030\000\033\000\034\000\
\064\000\065\000\025\000\034\000\106\000\031\000\001\000\052\000\
\053\000\110\000\055\000\006\000\007\000\008\000\057\000\049\000\
\003\000\061\000\133\000\023\000\088\000\034\000\026\000\068\000\
\069\000\070\000\071\000\072\000\073\000\074\000\075\000\094\000\
\050\000\051\000\049\000\080\000\128\000\083\000\085\000\009\000\
\010\000\011\000\012\000\013\000\014\000\018\000\019\000\020\000\
\061\000\021\000\032\000\050\000\051\000\095\000\096\000\097\000\
\066\000\067\000\100\000\101\000\075\000\104\000\035\000\105\000\
\107\000\018\000\019\000\060\000\046\000\021\000\036\000\112\000\
\054\000\037\000\056\000\059\000\058\000\018\000\019\000\020\000\
\119\000\021\000\132\000\077\000\078\000\079\000\123\000\038\000\
\039\000\040\000\084\000\091\000\098\000\126\000\099\000\103\000\
\108\000\130\000\111\000\113\000\109\000\142\000\114\000\115\000\
\118\000\092\000\116\000\041\000\042\000\043\000\044\000\120\000\
\139\000\121\000\122\000\141\000\018\000\019\000\020\000\127\000\
\021\000\124\000\129\000\131\000\135\000\143\000\038\000\039\000\
\040\000\134\000\136\000\137\000\138\000\016\000\102\000\140\000\
\003\000\018\000\047\000\021\000\125\000\086\000\093\000\000\000\
\000\000\000\000\041\000\042\000\043\000\044\000"

let yycheck = "\006\000\
\002\001\091\000\002\001\002\001\003\001\002\001\013\000\014\000\
\033\000\034\000\010\001\006\001\079\000\010\001\001\000\023\000\
\024\000\084\000\026\000\007\001\008\001\009\001\029\000\003\001\
\005\001\032\000\116\000\002\001\030\001\024\001\002\001\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\064\000\
\020\001\021\001\003\001\023\001\111\000\052\000\054\000\035\001\
\036\001\037\001\038\001\039\001\040\001\001\001\002\001\003\001\
\063\000\005\001\002\001\020\001\021\001\068\000\069\000\070\000\
\036\000\037\000\073\000\074\000\075\000\077\000\006\001\078\000\
\080\000\001\001\002\001\003\001\002\001\005\001\024\001\086\000\
\002\001\024\001\033\001\002\001\005\001\001\001\002\001\003\001\
\095\000\005\001\115\000\025\001\006\001\026\001\101\000\011\001\
\012\001\013\001\005\001\005\001\004\001\109\000\004\001\004\001\
\027\001\113\000\005\001\025\001\028\001\134\000\002\001\006\001\
\002\001\029\001\026\001\031\001\032\001\033\001\034\001\004\001\
\127\000\004\001\004\001\131\000\001\001\002\001\003\001\006\001\
\005\001\004\001\004\001\025\001\004\001\140\000\011\001\012\001\
\013\001\006\001\004\001\004\001\004\001\028\001\075\000\006\001\
\006\001\006\001\004\001\006\001\108\000\056\000\063\000\255\255\
\255\255\255\255\031\001\032\001\033\001\034\001"

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
  TYPEVEC\000\
  SEMICOLON\000\
  COLON\000\
  COMMA\000\
  MUL\000\
  ARROW\000\
  ADR\000\
  VARP\000\
  ALLOC\000\
  LEN\000\
  NTH\000\
  VSET\000\
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
# 290 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 45 "parser.mly"
                      ( _2 )
# 297 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 49 "parser.mly"
       ( ASTStat(_1) )
# 304 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 50 "parser.mly"
                     ( ASTDefCmd(_1,_3) )
# 312 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 51 "parser.mly"
                      ( ASTStatCmd(_1,_3) )
# 320 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 55 "parser.mly"
                       ( ASTConst(_2,_3,_4))
# 329 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 56 "parser.mly"
                                    ( ASTFun(_2,_3,_5,_7) )
# 339 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 57 "parser.mly"
                                        ( ASTRec(_3,_4,_6,_8)  )
# 349 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 58 "parser.mly"
                ( ASTAssign(_2,_3))
# 357 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'argsp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 59 "parser.mly"
                                   (ASTProc(_2,_4,_6))
# 366 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'argsp) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 60 "parser.mly"
                                       (ASTProcRec(_3,_5,_7))
# 375 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
           (TYPEBOOL)
# 381 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
          (TYPEINT)
# 387 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 66 "parser.mly"
                            ( ASTArrow(_2, _4) )
# 395 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 67 "parser.mly"
                        (ASTVec(_3))
# 402 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 71 "parser.mly"
      ( ASTType(_1) )
# 409 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 72 "parser.mly"
                (ASTMul(_1,_3))
# 417 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 76 "parser.mly"
      ( ASTArg(_1) )
# 424 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 77 "parser.mly"
                 ( ASTArgs(_1, _3) )
# 432 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 81 "parser.mly"
                  ( _1,_3 )
# 440 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'argp) in
    Obj.repr(
# 85 "parser.mly"
       ( ASTArgp(_1))
# 447 "parser.ml"
               : 'argsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'argp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'argsp) in
    Obj.repr(
# 86 "parser.mly"
                     ( ASTArgsp(_1,_3) )
# 455 "parser.ml"
               : 'argsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 90 "parser.mly"
                  (ASTVarp0(_1,_3) )
# 463 "parser.ml"
               : 'argp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 91 "parser.mly"
                       (ASTVarp(_2,_4) )
# 471 "parser.ml"
               : 'argp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 95 "parser.mly"
            ( ASTEcho(_2) )
# 478 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'lval) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 96 "parser.mly"
                ( ASTSet(_2,_3) )
# 486 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 97 "parser.mly"
                         (ASTIfMaj(_2,_3,_4) )
# 495 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 98 "parser.mly"
                   ( ASTWhile(_2,_3) )
# 503 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 99 "parser.mly"
                    ( ASTCall (_2,_3) )
# 511 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 104 "parser.mly"
        (LvalId(_1))
# 518 "parser.ml"
               : 'lval))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'lval) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 105 "parser.mly"
                            (ASTLval(_3,_4))
# 526 "parser.ml"
               : 'lval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 109 "parser.mly"
      ( ASTExpr(_1) )
# 533 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 110 "parser.mly"
                       ( ASTAdr(_3) )
# 540 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exprp) in
    Obj.repr(
# 114 "parser.mly"
        (ASTExprp(_1))
# 547 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exprp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 115 "parser.mly"
               (ASTExprsp(_1,_2))
# 555 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 119 "parser.mly"
      ( ASTNum(_1) )
# 562 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 120 "parser.mly"
        ( ASTId(_1) )
# 569 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 121 "parser.mly"
                              ( ASTIf(_3, _4, _5) )
# 578 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 122 "parser.mly"
                          ( ASTAnd(_3, _4) )
# 586 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 123 "parser.mly"
                         ( ASTOr(_3, _4) )
# 594 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 124 "parser.mly"
                        ( ASTApp(_2, _3) )
# 602 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 125 "parser.mly"
                      ( ASTBra(_2, _4) )
# 610 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 126 "parser.mly"
                       ( ASTAlloc(_3) )
# 617 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 127 "parser.mly"
                     ( ASTLen(_3) )
# 624 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 128 "parser.mly"
                          ( ASTNth(_3,_4) )
# 632 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 129 "parser.mly"
                                ( ASTVSet(_3,_4,_5) )
# 641 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 133 "parser.mly"
       ( [_1] )
# 648 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 134 "parser.mly"
             ( _1::_2 )
# 656 "parser.ml"
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
