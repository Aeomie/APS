

(* AST Nodes *)

type ttype = 
| ASTStype of stype
| ASTArrow of ttype list * ttype

and stype = 
  | ASTBool
  | ASTInt
  | ASTVec of stype
and arg = 
  | ASTArg of string * ttype

and argp = 
  | ASTArgP of string * ttype
  | ASTArgPAddress of string *ttype

and exprp = 
  | ASTExpr of expr
  | ASTExprAddress of lval

and lval = 
  | ASTLval of string
  | ASTNthLval of lval * expr

and expr = 
  ASTNum of int
  | ASTId of string
  | ASTIf of expr * expr * expr
  | ASTApp of expr * exprp list
  | ASTLambda of arg list * expr

and stat = 
  | ASTEcho of expr
  | ASTSet of lval * expr
  | ASTIfB of expr * block * block
  | ASTWhile of expr * block
  | ASTCall of string * exprp list

and def = 
| ASTConst of string * ttype * expr
| ASTFun of string * ttype * arg list * expr
| ASTFunRec of string * ttype * arg list * expr
| ASTVar of string * stype
| ASTProc of string * argp list * block
| ASTProcRec of string * argp list * block
| ASTFunBlock of string * ttype * argp list * block
| ASTFunRecBlock of string * ttype * argp list * block

and return = 
  | ASTReturn of expr
and cmd = 
  | ASTStat of stat
  | ASTRet of return
  | ASTDef of def * cmd
  | ASTStatcmds of stat * cmd

and block = 
  | ASTBlock of cmd



