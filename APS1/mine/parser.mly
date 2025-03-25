%{
    open Ast
%}

%token <int> NUM
%token <string> IDENT
%token LBRA RBRA
%token LPAR RPAR

%token SEMIC DP COMMA STAR ARROW

%token CONST FUN REC ECHO IF AND OR BOOL INT

%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.cmd> cmds
%type <Ast.ttype> type
%type <Ast.ttype list> types
%type <Ast.arg> arg
%type <Ast.arg list> args
%type <Ast.def> def
%type <Ast.stat> stat

%type <Ast.cmd> prog
%start prog

%%
prog: 
    LBRA cmds RBRA { $2 }
;

cmds:
    stat { ASTStat $1 }
    | def SEMIC cmds { ASTDef($1,$3)}
;

stat:
    ECHO expr { ASTEcho($2) }
;

def:
    CONST IDENT type expr { ASTConst($2, $3 , $4) }
    | FUN IDENT type LBRA args RBRA expr { ASTFun($2, $3 , $5, $7) }
    | FUN REC IDENT type LBRA args RBRA expr { ASTFunRec($3, $4, $6, $8) }
;

types:
    type { [$1] }
    |  type STAR types { $1::$3 }
; 
type:
    BOOL { ASTBool }
    | INT { ASTInt }
    | LPAR types ARROW type RPAR { ASTArrow($2, $4) }
;

arg:
    IDENT DP type { ASTArg($1, $3) }
;

args : 
    arg { [$1] }
    | arg COMMA args { $1::$3 }
;

expr:
    NUM { ASTNum($1)}
|   IDENT { ASTId($1) }
|   LPAR IF expr expr expr RPAR { ASTIf($3, $4, $5) }
|   LPAR AND expr expr RPAR { ASTAnd($3, $4) }
|   LPAR OR expr expr RPAR { ASTOr($3, $4) }
|   LPAR expr exprs RPAR { ASTApp($2, $3) }
|   LBRA args RBRA expr { ASTLambda($2,$4) }
;

exprs:
    expr { [$1] }
|   expr exprs { $1::$2 }
;