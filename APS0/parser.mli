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

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.cmds
