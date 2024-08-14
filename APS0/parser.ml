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

# 45 "parser.ml"
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
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\004\000\005\000\003\000\003\000\007\000\007\000\007\000\008\000\
\008\000\008\000\010\000\010\000\009\000\009\000\011\000\006\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000\
\002\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\004\000\007\000\008\000\001\000\
\001\000\005\000\001\000\003\000\001\000\003\000\003\000\002\000\
\001\000\001\000\006\000\005\000\005\000\004\000\004\000\001\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\026\000\001\000\000\000\000\000\000\000\
\000\000\003\000\000\000\017\000\018\000\000\000\000\000\016\000\
\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\008\000\009\000\000\000\
\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\005\000\000\000\000\000\
\000\000\000\000\000\000\025\000\022\000\015\000\023\000\014\000\
\000\000\000\000\000\000\000\000\000\000\020\000\021\000\012\000\
\000\000\000\000\000\000\019\000\010\000\006\000\000\000\007\000"

let yydgoto = "\002\000\
\039\000\040\000\009\000\004\000\005\000\010\000\011\000\044\000\
\027\000\045\000\028\000"

let yysindex = "\006\000\
\008\255\000\000\026\255\000\000\000\000\009\255\013\255\255\254\
\015\255\000\000\003\255\000\000\000\000\036\255\022\255\000\000\
\002\255\002\255\027\255\000\000\026\255\009\255\009\255\009\255\
\009\255\005\255\034\255\018\255\002\255\000\000\000\000\009\255\
\040\255\002\255\000\000\009\255\009\255\009\255\009\255\042\255\
\002\255\009\255\022\255\025\255\028\255\000\000\022\255\046\255\
\009\255\048\255\049\255\000\000\000\000\000\000\000\000\000\000\
\002\255\002\255\050\255\022\255\051\255\000\000\000\000\000\000\
\053\255\009\255\052\255\000\000\000\000\000\000\009\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\055\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\058\255\000\000\
\000\000\000\000\000\000\033\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\250\255\024\000\043\000\000\000\000\000\000\000\000\000\242\255\
\215\255\009\000\000\000"

let yytablesize = 66
let yytable = "\016\000\
\018\000\056\000\032\000\033\000\029\000\059\000\001\000\025\000\
\019\000\012\000\013\000\014\000\003\000\015\000\017\000\036\000\
\037\000\038\000\067\000\048\000\020\000\030\000\031\000\026\000\
\021\000\046\000\054\000\041\000\034\000\049\000\050\000\051\000\
\006\000\007\000\008\000\055\000\012\000\013\000\014\000\042\000\
\015\000\043\000\061\000\065\000\047\000\053\000\022\000\023\000\
\024\000\057\000\060\000\062\000\063\000\058\000\068\000\066\000\
\069\000\071\000\011\000\070\000\013\000\024\000\052\000\035\000\
\072\000\064\000"

let yycheck = "\006\000\
\002\001\043\000\017\000\018\000\003\001\047\000\001\000\014\000\
\010\001\001\001\002\001\003\001\005\001\005\001\002\001\022\000\
\023\000\024\000\060\000\034\000\006\001\020\001\021\001\002\001\
\022\001\032\000\041\000\023\001\002\001\036\000\037\000\038\000\
\007\001\008\001\009\001\042\000\001\001\002\001\003\001\006\001\
\005\001\024\001\049\000\058\000\005\001\004\001\011\001\012\001\
\013\001\025\001\005\001\004\001\004\001\026\001\004\001\006\001\
\004\001\006\001\026\001\066\000\006\001\004\001\039\000\021\000\
\071\000\057\000"

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
# 193 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 44 "parser.mly"
                      ( _2 )
# 200 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 48 "parser.mly"
       ( ASTStat(_1) )
# 207 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 49 "parser.mly"
                     ( ASTDefCmd(_1,_3) )
# 215 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 54 "parser.mly"
                       ( ASTConst(_2,_3,_4))
# 224 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 55 "parser.mly"
                                    ( ASTFun(_2,_3,_5,_7) )
# 234 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 56 "parser.mly"
                                        ( ASTRec(_3,_4,_6,_8)  )
# 244 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
           (TYPEBOOL)
# 250 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
          (TYPEINT)
# 256 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 65 "parser.mly"
                            ( ASTArrow(_2, _4) )
# 264 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 69 "parser.mly"
      ( ASTType(_1) )
# 271 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 70 "parser.mly"
                (ASTMul(_1,_3))
# 279 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 74 "parser.mly"
      ( ASTArg(_1) )
# 286 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 75 "parser.mly"
                 ( ASTArgs(_1, _3) )
# 294 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 79 "parser.mly"
                  ( _1,_3 )
# 302 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 83 "parser.mly"
            ( ASTEcho(_2) )
# 309 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 91 "parser.mly"
      ( ASTNum(_1) )
# 316 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "parser.mly"
        ( ASTId(_1) )
# 323 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 93 "parser.mly"
                              ( ASTIf(_3, _4, _5) )
# 332 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 94 "parser.mly"
                          ( ASTAnd(_3, _4) )
# 340 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 95 "parser.mly"
                         ( ASTOr(_3, _4) )
# 348 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 96 "parser.mly"
                        ( ASTApp(_2, _3) )
# 356 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 97 "parser.mly"
                      ( ASTBra(_2, _4) )
# 364 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 101 "parser.mly"
       ( [_1] )
# 371 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 102 "parser.mly"
             ( _1::_2 )
# 379 "parser.ml"
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
