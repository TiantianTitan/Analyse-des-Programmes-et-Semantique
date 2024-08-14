(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: lexer.mll                                                   == *)
(* ==  Lexique                                                             == *)
(* ========================================================================== *)

{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof

}
rule token = parse
    [' ' '\t' '\r' '\n']       { token lexbuf }     (* skip blanks *)
  | '['              { LBRA }
  | ']'              { RBRA }
  | '('              { LPAR }
  | ')'              { RPAR }
  | ';'              { SEMICOLON }
  | ':'              { COLON }
  | ','              { COMMA }
  | '*'              { MUL }
  | "->"             { ARROW }
  | "if"             { IF }
  | "and"            { AND }
  | "or"             { OR }
  | "int"            { TYPEINT }
  | "bool"           { TYPEBOOL }
  | "ECHO"           { ECHO }
  | "CONST"          { CONST }
  | "FUN"            { FUN }
  | "REC"            { REC } (* "FUN REC" *)
  
  (* APS1 *)
  | "VAR"            { VAR } 
  | "SET"            { SET }
  | "PROC"           { PROC }
  | "CALL"           { CALL }
  | "IF"             { IFMAJ }
  | "WHILE"          { WHILE }

  (* APS1a *)
  | "var"            { VARP }
  | "adr"            { ADR }
  | "void"           { TYPEVOID }

  (* APS2 *)
  | "len"            { LEN }
  | "nth"            { NTH }
  | "alloc"          { ALLOC }
  | "vec"            { TYPEVEC }
  | "vset"           { VSET }

  | ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
  | eof              { raise Eof }
