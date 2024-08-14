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

  | ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
  | eof              { raise Eof }
