
{
    open Parser
    exception Eof
}


rule token = parse
    [' ' '\t' '\n'] { token lexbuf } (* skip blank *)
    | '[' { LBRA }
    | ']' { RBRA }
    | '(' { LPAR }
    | ')' { RPAR }
    | "->" { ARROW }
    | "*" { STAR }
    | ";" { SEMIC }
    | ":" { DP } 
    | "," { COMMA }
    | "ECHO"           { ECHO } 
    | "CONST"   { CONST}
    | "FUN" {FUN}
    | "REC"     {REC}  
    | "bool"    { BOOL }
    | "int"     { INT } 
    | "if"  { IF }
    | "and"     { AND }
    | "or"  { OR }
    | "true" | "false" | "not" | "eq" | "lt" | "add" | "sub" | "mul" | "div" as lxm { IDENT(lxm) }
    | ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
    | ['a'-'z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
    | eof              { raise Eof }