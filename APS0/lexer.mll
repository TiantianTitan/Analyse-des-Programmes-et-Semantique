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
  
  (* TME2 *)
  (* | "var"            { VAR } *)
  (* | "set"            { SET }
  | "proc"           { PROC }
  | "call"           { CALL }
  | "IF"             { IFMAJ }
  | "while"          { WHILE } *)


  | ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
  | eof              { raise Eof }
