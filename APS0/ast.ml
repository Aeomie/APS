

(* AST Nodes *)

type ttype = 
| ASTBool
| ASTInt
| ASTArrow of ttype list * ttype

type arg = 
  | ASTArg of string * ttype

type expr = 
  ASTNum of int
  | ASTId of string
  | ASTApp of expr * expr list
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTIf of expr * expr * expr
  | ASTLambda of arg list * expr

type stat = 
  | ASTEcho of expr

type def = 
| ASTConst of string * ttype * expr
| ASTFun of string * ttype * arg list * expr
| ASTFunRec of string * ttype * arg list * expr

type cmd = 
  | ASTStat of stat
  | ASTDef of def * cmd


