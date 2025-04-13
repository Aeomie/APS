
{
    open Parser
    exception Eof
}


rule token = parse
    | [' ' '\t' '\r' '\n'] { token lexbuf } (* skip blank *)
    | '[' { LBRA }
    | ']' { RBRA }
    | '(' { LPAR }
    | ')' { RPAR }
    | ";" { SEMIC }
    | ":" { DP } 
    | "," { COMMA }
    | "*" { STAR }
    | "->" { ARROW }
    | "CONST"   { CONST}
    | "FUN" {FUN}
    | "REC"     {REC}  
    | "VAR" {VAR}
    | "PROC" {PROC}
    | "ECHO"           { ECHO } 
    | "SET" {SET}
    | "IF" {IFB}
    | "WHILE" {WHILE}
    | "CALL" {CALL}
    | "if"  { IF }
    | "and"     { AND }
    | "or"  { OR }
    | "bool"    { BOOL }
    | "int"     { INT }
    | "var"     { IDVar }
    | "adr"     { ADR }
    | "true" | "false" | "not" | "eq" | "lt" | "add" | "sub" | "mul" | "div" as lxm { IDENT(lxm) }
    | "alloc" {ALLOC}
    | "nth" {NTH}
    | "len" {LEN}
    | "vset" {VSET}
    | "vec" {VEC}
    | ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
    | ['a'-'z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
    | eof              { raise Eof }