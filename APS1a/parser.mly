%{
    open Ast
%}

%token <int> NUM
%token <string> IDENT
%token LBRA RBRA
%token LPAR RPAR

%token SEMIC DP COMMA STAR ARROW

%token CONST FUN REC VAR PROC
%token ECHO SET IFB WHILE CALL
%token IDVar ADR
%token IF AND OR BOOL INT


%type <Ast.expr> expr
%type <Ast.expr list> exprs

%type <Ast.exprp> exprp
%type <Ast.exprp list> exprsp

%type <Ast.cmd> cmds
%type <Ast.ttype> type
%type <Ast.ttype list> types

%type <Ast.arg> arg
%type <Ast.arg list> args

%type <Ast.argp> argp
%type <Ast.argp list> argsp

%type <Ast.def> def
%type <Ast.stat> stat

%type <Ast.block> block
%type <Ast.block> prog

%start prog

%%
prog: 
    block { $1 }
;

block: 
    LBRA cmds RBRA { ASTBlock($2)}
;
cmds:
    stat { ASTStat $1 }
    | def SEMIC cmds { ASTDef($1,$3)}
    | stat SEMIC cmds { ASTStatcmds($1,$3)}
;

stat:
    ECHO expr { ASTEcho($2) }
    | SET IDENT expr { ASTSet($2,$3) }
    | IFB expr block block { ASTIfB($2,$3,$4) }
    | WHILE expr block { ASTWhile($2,$3) }
    | CALL IDENT exprsp { ASTCall($2,$3) }
;

def:
    CONST IDENT type expr { ASTConst($2, $3 , $4) }
    | FUN IDENT type LBRA args RBRA expr { ASTFun($2, $3 , $5, $7) }
    | FUN REC IDENT type LBRA args RBRA expr { ASTFunRec($3, $4, $6, $8) }
    | VAR IDENT type { ASTVar($2,$3)}
    | PROC IDENT LBRA argsp RBRA block { ASTProc($2,$4,$6)}
    | PROC REC IDENT LBRA argsp RBRA block { ASTProcRec($3,$5,$7)}
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
args: 
    arg { [$1] }
    | arg COMMA args { $1::$3 }
;

argp:
    IDENT DP type { ASTArgP($1, $3)}
    | IDVar IDENT DP type { ASTArgPAddress($2,$4)}
;

argsp:
    argp { [$1] }
    | argp COMMA argsp { $1::$3 }
;

exprp:
    | expr { ASTExpr($1) }
    | LPAR ADR IDENT RPAR { ASTExprAddress($3) }
;
exprsp:
    | exprp { [$1] } 
    | exprp exprsp { $1::$2 }
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