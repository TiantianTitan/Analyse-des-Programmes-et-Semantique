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

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.cmds
