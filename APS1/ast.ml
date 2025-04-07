

(* AST Nodes *)

type ttype = 
| ASTBool
| ASTInt
| ASTArrow of ttype list * ttype

and arg = 
  | ASTArg of string * ttype

and expr = 
  ASTNum of int
  | ASTId of string
  | ASTApp of expr * expr list
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTIf of expr * expr * expr
  | ASTLambda of arg list * expr

and stat = 
  | ASTEcho of expr
  | ASTSet of string * expr
  | ASTIfB of expr * block * block
  | ASTWhile of expr * block
  | ASTCall of string * expr list

and def = 
| ASTConst of string * ttype * expr
| ASTFun of string * ttype * arg list * expr
| ASTFunRec of string * ttype * arg list * expr
| ASTVar of string * ttype
| ASTProc of string * arg list * block
| ASTProcRec of string * arg list * block

and cmd = 
  | ASTStat of stat
  | ASTDef of def * cmd
  | ASTStatcmds of stat * cmd

and block = 
  | ASTBlock of cmd



