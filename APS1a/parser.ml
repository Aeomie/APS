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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    open Ast
# 36 "parser.ml"
let yytransl_const = [|
  259 (* LBRA *);
  260 (* RBRA *);
  261 (* LPAR *);
  262 (* RPAR *);
  263 (* SEMIC *);
  264 (* DP *);
  265 (* COMMA *);
  266 (* STAR *);
  267 (* ARROW *);
  268 (* CONST *);
  269 (* FUN *);
  270 (* REC *);
  271 (* VAR *);
  272 (* PROC *);
  273 (* ECHO *);
  274 (* SET *);
  275 (* IFB *);
  276 (* WHILE *);
  277 (* CALL *);
  278 (* IDVar *);
  279 (* ADR *);
  280 (* IF *);
  281 (* AND *);
  282 (* OR *);
  283 (* BOOL *);
  284 (* INT *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\015\000\014\000\005\000\005\000\005\000\013\000\013\000\013\000\
\013\000\013\000\012\000\012\000\012\000\012\000\012\000\012\000\
\007\000\007\000\006\000\006\000\006\000\008\000\009\000\009\000\
\010\000\010\000\011\000\011\000\003\000\003\000\004\000\004\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000\
\002\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\002\000\003\000\004\000\
\003\000\003\000\004\000\007\000\008\000\003\000\006\000\007\000\
\001\000\003\000\001\000\001\000\005\000\003\000\001\000\003\000\
\003\000\004\000\001\000\003\000\001\000\004\000\001\000\002\000\
\001\000\001\000\006\000\005\000\005\000\004\000\004\000\001\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\001\000\042\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\033\000\
\034\000\000\000\000\000\006\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\019\000\020\000\000\000\000\000\
\000\000\014\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\007\000\000\000\009\000\000\000\029\000\
\000\000\010\000\004\000\005\000\000\000\000\000\011\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\008\000\000\000\
\032\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\022\000\024\000\039\000\000\000\000\000\000\000\
\041\000\038\000\000\000\018\000\000\000\000\000\000\000\025\000\
\000\000\028\000\015\000\000\000\000\000\036\000\037\000\030\000\
\021\000\012\000\000\000\026\000\016\000\035\000\013\000"

let yydgoto = "\002\000\
\051\000\078\000\057\000\058\000\015\000\061\000\062\000\046\000\
\047\000\068\000\069\000\016\000\017\000\004\000\005\000"

let yysindex = "\013\000\
\018\255\000\000\081\255\000\000\000\000\028\255\006\255\029\255\
\014\255\075\255\040\255\075\255\075\255\046\255\051\255\037\255\
\052\255\252\254\252\254\054\255\252\254\050\255\058\255\000\000\
\000\000\059\255\049\255\000\000\075\255\018\255\018\255\080\255\
\000\000\081\255\081\255\252\254\000\000\000\000\075\255\067\255\
\252\254\000\000\004\255\068\255\071\255\053\255\082\255\075\255\
\075\255\075\255\075\255\000\000\018\255\000\000\010\255\000\000\
\080\255\000\000\000\000\000\000\078\255\079\255\000\000\059\255\
\086\255\087\255\101\255\095\255\102\255\004\255\252\254\059\255\
\075\255\075\255\075\255\075\255\075\255\103\255\000\000\105\255\
\000\000\252\254\252\254\104\255\059\255\252\254\106\255\004\255\
\018\255\107\255\000\000\000\000\000\000\075\255\109\255\110\255\
\000\000\000\000\111\255\000\000\112\255\075\255\108\255\000\000\
\252\254\000\000\000\000\018\255\113\255\000\000\000\000\000\000\
\000\000\000\000\075\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\116\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\117\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\042\255\000\000\000\000\000\000\099\255\000\000\000\000\000\000\
\000\000\000\000\000\000\118\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\119\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\246\255\036\000\000\000\066\000\239\255\242\255\042\000\000\000\
\229\255\000\000\211\255\000\000\000\000\235\255\000\000"

let yytablesize = 125
let yytable = "\028\000\
\036\000\030\000\031\000\039\000\040\000\066\000\042\000\019\000\
\053\000\054\000\024\000\025\000\026\000\001\000\027\000\022\000\
\059\000\060\000\052\000\020\000\003\000\056\000\037\000\038\000\
\090\000\067\000\065\000\023\000\063\000\018\000\021\000\079\000\
\080\000\048\000\049\000\050\000\084\000\074\000\075\000\076\000\
\077\000\029\000\106\000\034\000\092\000\031\000\056\000\032\000\
\031\000\024\000\025\000\026\000\043\000\027\000\033\000\041\000\
\091\000\103\000\035\000\044\000\045\000\072\000\093\000\094\000\
\095\000\096\000\077\000\107\000\101\000\064\000\070\000\104\000\
\048\000\049\000\050\000\024\000\025\000\026\000\071\000\027\000\
\024\000\025\000\026\000\109\000\055\000\073\000\117\000\082\000\
\085\000\083\000\116\000\114\000\006\000\007\000\086\000\008\000\
\009\000\010\000\011\000\012\000\013\000\014\000\087\000\088\000\
\119\000\089\000\099\000\102\000\098\000\017\000\108\000\115\000\
\097\000\105\000\110\000\111\000\112\000\113\000\118\000\003\000\
\023\000\027\000\081\000\100\000\040\000"

let yycheck = "\010\000\
\005\001\012\000\013\000\018\000\019\000\002\001\021\000\002\001\
\030\000\031\000\001\001\002\001\003\001\001\000\005\001\002\001\
\034\000\035\000\029\000\014\001\003\001\032\000\027\001\028\001\
\070\000\022\001\041\000\014\001\039\000\002\001\002\001\053\000\
\023\001\024\001\025\001\026\001\064\000\048\000\049\000\050\000\
\051\000\002\001\088\000\007\001\072\000\004\001\057\000\002\001\
\007\001\001\001\002\001\003\001\003\001\005\001\004\001\002\001\
\071\000\085\000\007\001\002\001\002\001\009\001\073\000\074\000\
\075\000\076\000\077\000\089\000\083\000\003\001\003\001\086\000\
\024\001\025\001\026\001\001\001\002\001\003\001\008\001\005\001\
\001\001\002\001\003\001\094\000\005\001\004\001\108\000\010\001\
\003\001\011\001\105\000\102\000\012\001\013\001\008\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\002\001\009\001\
\115\000\004\001\002\001\004\001\006\001\011\001\004\001\004\001\
\077\000\008\001\006\001\006\001\006\001\006\001\006\001\004\001\
\004\001\004\001\057\000\082\000\006\001"

let yynames_const = "\
  LBRA\000\
  RBRA\000\
  LPAR\000\
  RPAR\000\
  SEMIC\000\
  DP\000\
  COMMA\000\
  STAR\000\
  ARROW\000\
  CONST\000\
  FUN\000\
  REC\000\
  VAR\000\
  PROC\000\
  ECHO\000\
  SET\000\
  IFB\000\
  WHILE\000\
  CALL\000\
  IDVar\000\
  ADR\000\
  IF\000\
  AND\000\
  OR\000\
  BOOL\000\
  INT\000\
  "

let yynames_block = "\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 44 "parser.mly"
          ( _1 )
# 224 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd) in
    Obj.repr(
# 48 "parser.mly"
                   ( ASTBlock(_2))
# 231 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 51 "parser.mly"
         ( ASTStat _1 )
# 238 "parser.ml"
               : Ast.cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd) in
    Obj.repr(
# 52 "parser.mly"
                     ( ASTDef(_1,_3))
# 246 "parser.ml"
               : Ast.cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd) in
    Obj.repr(
# 53 "parser.mly"
                      ( ASTStatcmds(_1,_3))
# 254 "parser.ml"
               : Ast.cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 57 "parser.mly"
              ( ASTEcho(_2) )
# 261 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 58 "parser.mly"
                     ( ASTSet(_2,_3) )
# 269 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 59 "parser.mly"
                           ( ASTIfB(_2,_3,_4) )
# 278 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 60 "parser.mly"
                       ( ASTWhile(_2,_3) )
# 286 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprp list) in
    Obj.repr(
# 61 "parser.mly"
                        ( ASTCall(_2,_3) )
# 294 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.ttype) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 65 "parser.mly"
                          ( ASTConst(_2, _3 , _4) )
# 303 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.ttype) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 66 "parser.mly"
                                         ( ASTFun(_2, _3 , _5, _7) )
# 313 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.ttype) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 67 "parser.mly"
                                             ( ASTFunRec(_3, _4, _6, _8) )
# 323 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.ttype) in
    Obj.repr(
# 68 "parser.mly"
                     ( ASTVar(_2,_3))
# 331 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.argp list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 69 "parser.mly"
                                       ( ASTProc(_2,_4,_6))
# 340 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.argp list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 70 "parser.mly"
                                           ( ASTProcRec(_3,_5,_7))
# 349 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.ttype) in
    Obj.repr(
# 74 "parser.mly"
         ( [_1] )
# 356 "parser.ml"
               : Ast.ttype list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.ttype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.ttype list) in
    Obj.repr(
# 75 "parser.mly"
                       ( _1::_3 )
# 364 "parser.ml"
               : Ast.ttype list))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
         ( ASTBool )
# 370 "parser.ml"
               : Ast.ttype))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
          ( ASTInt )
# 376 "parser.ml"
               : Ast.ttype))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.ttype list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.ttype) in
    Obj.repr(
# 80 "parser.mly"
                                 ( ASTArrow(_2, _4) )
# 384 "parser.ml"
               : Ast.ttype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.ttype) in
    Obj.repr(
# 84 "parser.mly"
                  ( ASTArg(_1, _3) )
# 392 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 87 "parser.mly"
        ( [_1] )
# 399 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg list) in
    Obj.repr(
# 88 "parser.mly"
                     ( _1::_3 )
# 407 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.ttype) in
    Obj.repr(
# 92 "parser.mly"
                  ( ASTArgP(_1, _3))
# 415 "parser.ml"
               : Ast.argp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.ttype) in
    Obj.repr(
# 93 "parser.mly"
                          ( ASTArgPAddress(_2,_4))
# 423 "parser.ml"
               : Ast.argp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.argp) in
    Obj.repr(
# 97 "parser.mly"
         ( [_1] )
# 430 "parser.ml"
               : Ast.argp list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.argp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.argp list) in
    Obj.repr(
# 98 "parser.mly"
                       ( _1::_3 )
# 438 "parser.ml"
               : Ast.argp list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 102 "parser.mly"
           ( ASTExpr(_1) )
# 445 "parser.ml"
               : Ast.exprp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 103 "parser.mly"
                          ( ASTExprAddress(_3) )
# 452 "parser.ml"
               : Ast.exprp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprp) in
    Obj.repr(
# 106 "parser.mly"
            ( [_1] )
# 459 "parser.ml"
               : Ast.exprp list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.exprp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprp list) in
    Obj.repr(
# 107 "parser.mly"
                   ( _1::_2 )
# 467 "parser.ml"
               : Ast.exprp list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 112 "parser.mly"
        ( ASTNum(_1))
# 474 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 113 "parser.mly"
          ( ASTId(_1) )
# 481 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 114 "parser.mly"
                                ( ASTIf(_3, _4, _5) )
# 490 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 115 "parser.mly"
                            ( ASTAnd(_3, _4) )
# 498 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 116 "parser.mly"
                           ( ASTOr(_3, _4) )
# 506 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 117 "parser.mly"
                         ( ASTApp(_2, _3) )
# 514 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 118 "parser.mly"
                        ( ASTLambda(_2,_4) )
# 522 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 122 "parser.mly"
         ( [_1] )
# 529 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 123 "parser.mly"
               ( _1::_2 )
# 537 "parser.ml"
               : Ast.expr list))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.block)
