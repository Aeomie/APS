type token =
  | NUM of (int)
  | IDENT of (string)
  | LBRA
  | RBRA
  | LPAR
  | RPAR
  | SEMIC
  | DP
  | COMMA
  | STAR
  | ARROW
  | CONST
  | FUN
  | REC
  | VAR
  | PROC
  | ECHO
  | SET
  | IFB
  | WHILE
  | CALL
  | IDVar
  | ADR
  | IF
  | AND
  | OR
  | BOOL
  | INT

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.block
