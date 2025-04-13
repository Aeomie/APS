

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
  | ASTExprAddress of string

and lval = 
  | ASTLval of string
  | ASTNthLval of lval * expr

and expr = 
  ASTNum of int
  | ASTId of string
  | ASTApp of expr * expr list
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTIf of expr * expr * expr
  | ASTLambda of arg list * expr
  | ASTAlloc of  expr
  | ASTLen of expr
  | ASTNth of expr * expr
  | ASTVset of expr * expr * expr

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

and cmd = 
  | ASTStat of stat
  | ASTDef of def * cmd
  | ASTStatcmds of stat * cmd

and block = 
  | ASTBlock of cmd



