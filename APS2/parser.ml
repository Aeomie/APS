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
  | ALLOC
  | NTH
  | LEN
  | VSET
  | VEC

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    open Ast
# 41 "parser.ml"
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
  285 (* ALLOC *);
  286 (* NTH *);
  287 (* LEN *);
  288 (* VSET *);
  289 (* VEC *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\017\000\016\000\006\000\006\000\006\000\015\000\015\000\015\000\
\015\000\015\000\014\000\014\000\014\000\014\000\014\000\014\000\
\008\000\008\000\007\000\007\000\009\000\009\000\009\000\010\000\
\011\000\011\000\012\000\012\000\013\000\013\000\003\000\003\000\
\004\000\004\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\002\000\002\000\005\000\
\005\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\002\000\003\000\004\000\
\003\000\003\000\004\000\007\000\008\000\003\000\006\000\007\000\
\001\000\003\000\001\000\005\000\001\000\001\000\004\000\003\000\
\001\000\003\000\003\000\004\000\001\000\003\000\001\000\004\000\
\001\000\002\000\001\000\001\000\006\000\005\000\005\000\004\000\
\004\000\004\000\004\000\005\000\006\000\001\000\002\000\001\000\
\005\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\001\000\050\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\035\000\
\036\000\000\000\000\000\006\000\048\000\000\000\000\000\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\021\000\022\000\
\000\000\019\000\000\000\000\000\000\000\014\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\007\000\000\000\009\000\000\000\
\031\000\000\000\010\000\004\000\005\000\000\000\000\000\000\000\
\011\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\008\000\000\000\034\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\024\000\026\000\041\000\000\000\000\000\000\000\
\042\000\000\000\043\000\000\000\047\000\040\000\000\000\000\000\
\023\000\018\000\000\000\000\000\000\000\027\000\000\000\030\000\
\015\000\000\000\000\000\038\000\039\000\044\000\000\000\049\000\
\032\000\020\000\012\000\000\000\028\000\016\000\037\000\045\000\
\013\000"

let yydgoto = "\002\000\
\059\000\092\000\066\000\067\000\031\000\015\000\071\000\072\000\
\042\000\050\000\051\000\078\000\079\000\016\000\017\000\004\000\
\005\000"

let yysindex = "\008\000\
\022\255\000\000\103\255\000\000\000\000\052\255\013\255\064\255\
\051\255\015\255\017\255\015\255\015\255\069\255\073\255\054\255\
\077\255\059\255\059\255\088\255\084\255\098\255\100\255\000\000\
\000\000\101\255\067\255\000\000\000\000\075\255\015\255\022\255\
\022\255\057\255\000\000\103\255\103\255\024\255\000\000\000\000\
\015\255\000\000\104\255\059\255\076\255\000\000\006\255\105\255\
\102\255\108\255\121\255\015\255\015\255\015\255\015\255\015\255\
\015\255\015\255\015\255\017\255\000\000\022\255\000\000\009\255\
\000\000\057\255\000\000\000\000\000\000\084\255\116\255\117\255\
\000\000\101\255\124\255\123\255\127\255\125\255\128\255\006\255\
\059\255\101\255\015\255\015\255\015\255\015\255\129\255\015\255\
\130\255\015\255\015\255\131\255\015\255\000\000\136\255\000\000\
\133\255\059\255\059\255\137\255\101\255\059\255\132\255\006\255\
\022\255\138\255\000\000\000\000\000\000\015\255\139\255\140\255\
\000\000\141\255\000\000\015\255\000\000\000\000\142\255\143\255\
\000\000\000\000\144\255\015\255\147\255\000\000\059\255\000\000\
\000\000\022\255\146\255\000\000\000\000\000\000\148\255\000\000\
\000\000\000\000\000\000\015\255\000\000\000\000\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\149\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\151\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\019\255\000\000\000\000\000\000\000\000\122\255\000\000\
\000\000\000\000\000\000\000\000\000\000\152\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\153\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\246\255\052\000\000\000\078\000\097\000\058\000\242\255\060\000\
\236\255\000\000\237\255\000\000\189\255\000\000\000\000\230\255\
\000\000"

let yytablesize = 159
let yytable = "\028\000\
\046\000\032\000\033\000\041\000\043\000\062\000\063\000\076\000\
\001\000\024\000\025\000\026\000\106\000\027\000\019\000\024\000\
\025\000\026\000\029\000\027\000\061\000\030\000\033\000\065\000\
\003\000\033\000\020\000\077\000\038\000\075\000\073\000\095\000\
\052\000\053\000\054\000\094\000\128\000\055\000\056\000\057\000\
\058\000\084\000\085\000\086\000\087\000\088\000\089\000\090\000\
\091\000\097\000\039\000\040\000\022\000\018\000\100\000\065\000\
\070\000\024\000\025\000\026\000\036\000\064\000\108\000\038\000\
\023\000\021\000\107\000\024\000\025\000\026\000\034\000\027\000\
\109\000\110\000\111\000\112\000\035\000\114\000\129\000\116\000\
\091\000\125\000\119\000\037\000\123\000\039\000\040\000\126\000\
\045\000\044\000\052\000\053\000\054\000\068\000\069\000\055\000\
\056\000\057\000\058\000\131\000\047\000\048\000\049\000\142\000\
\060\000\135\000\074\000\080\000\070\000\081\000\039\000\040\000\
\141\000\139\000\006\000\007\000\082\000\008\000\009\000\010\000\
\011\000\012\000\013\000\014\000\083\000\098\000\101\000\099\000\
\103\000\145\000\102\000\105\000\017\000\104\000\113\000\115\000\
\118\000\120\000\121\000\127\000\124\000\130\000\117\000\096\000\
\132\000\133\000\134\000\136\000\137\000\138\000\140\000\143\000\
\003\000\144\000\025\000\029\000\093\000\122\000\046\000"

let yycheck = "\010\000\
\021\000\012\000\013\000\018\000\019\000\032\000\033\000\002\001\
\001\000\001\001\002\001\003\001\080\000\005\001\002\001\001\001\
\002\001\003\001\002\001\005\001\031\000\005\001\004\001\034\000\
\003\001\007\001\014\001\022\001\005\001\044\000\041\000\023\001\
\024\001\025\001\026\001\062\000\104\000\029\001\030\001\031\001\
\032\001\052\000\053\000\054\000\055\000\056\000\057\000\058\000\
\059\000\070\000\027\001\028\001\002\001\002\001\074\000\066\000\
\033\001\001\001\002\001\003\001\007\001\005\001\082\000\005\001\
\014\001\002\001\081\000\001\001\002\001\003\001\002\001\005\001\
\083\000\084\000\085\000\086\000\004\001\088\000\105\000\090\000\
\091\000\101\000\093\000\007\001\099\000\027\001\028\001\102\000\
\005\001\002\001\024\001\025\001\026\001\036\000\037\000\029\001\
\030\001\031\001\032\001\110\000\003\001\002\001\002\001\130\000\
\030\001\116\000\003\001\003\001\033\001\008\001\027\001\028\001\
\127\000\124\000\012\001\013\001\009\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\004\001\010\001\003\001\011\001\
\002\001\140\000\008\001\004\001\011\001\009\001\006\001\006\001\
\006\001\002\001\006\001\008\001\004\001\004\001\091\000\066\000\
\006\001\006\001\006\001\006\001\006\001\006\001\004\001\006\001\
\004\001\006\001\004\001\004\001\060\000\098\000\006\001"

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
  ALLOC\000\
  NTH\000\
  LEN\000\
  VSET\000\
  VEC\000\
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
# 48 "parser.mly"
          ( _1 )
# 263 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd) in
    Obj.repr(
# 52 "parser.mly"
                   ( ASTBlock(_2))
# 270 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 55 "parser.mly"
         ( ASTStat _1 )
# 277 "parser.ml"
               : Ast.cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd) in
    Obj.repr(
# 56 "parser.mly"
                     ( ASTDef(_1,_3))
# 285 "parser.ml"
               : Ast.cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd) in
    Obj.repr(
# 57 "parser.mly"
                      ( ASTStatcmds(_1,_3))
# 293 "parser.ml"
               : Ast.cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 61 "parser.mly"
              ( ASTEcho(_2) )
# 300 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.lval) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 62 "parser.mly"
                    ( ASTSet(_2,_3) )
# 308 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 63 "parser.mly"
                           ( ASTIfB(_2,_3,_4) )
# 317 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 64 "parser.mly"
                       ( ASTWhile(_2,_3) )
# 325 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprp list) in
    Obj.repr(
# 65 "parser.mly"
                        ( ASTCall(_2,_3) )
# 333 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.ttype) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 69 "parser.mly"
                          ( ASTConst(_2, _3 , _4) )
# 342 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.ttype) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 70 "parser.mly"
                                         ( ASTFun(_2, _3 , _5, _7) )
# 352 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.ttype) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 71 "parser.mly"
                                             ( ASTFunRec(_3, _4, _6, _8) )
# 362 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.stype) in
    Obj.repr(
# 72 "parser.mly"
                      ( ASTVar(_2,_3))
# 370 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.argp list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 73 "parser.mly"
                                       ( ASTProc(_2,_4,_6))
# 379 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.argp list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 74 "parser.mly"
                                           ( ASTProcRec(_3,_5,_7))
# 388 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.ttype) in
    Obj.repr(
# 78 "parser.mly"
         ( [_1] )
# 395 "parser.ml"
               : Ast.ttype list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.ttype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.ttype list) in
    Obj.repr(
# 79 "parser.mly"
                       ( _1::_3 )
# 403 "parser.ml"
               : Ast.ttype list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stype) in
    Obj.repr(
# 82 "parser.mly"
            ( ASTStype(_1))
# 410 "parser.ml"
               : Ast.ttype))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.ttype list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.ttype) in
    Obj.repr(
# 83 "parser.mly"
                                 ( ASTArrow(_2, _4) )
# 418 "parser.ml"
               : Ast.ttype))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
         ( ASTBool )
# 424 "parser.ml"
               : Ast.stype))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
          ( ASTInt)
# 430 "parser.ml"
               : Ast.stype))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.stype) in
    Obj.repr(
# 89 "parser.mly"
                          (ASTVec(_3))
# 437 "parser.ml"
               : Ast.stype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.ttype) in
    Obj.repr(
# 91 "parser.mly"
                  ( ASTArg(_1, _3) )
# 445 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 94 "parser.mly"
        ( [_1] )
# 452 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg list) in
    Obj.repr(
# 95 "parser.mly"
                     ( _1::_3 )
# 460 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.ttype) in
    Obj.repr(
# 99 "parser.mly"
                  ( ASTArgP(_1, _3))
# 468 "parser.ml"
               : Ast.argp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.ttype) in
    Obj.repr(
# 100 "parser.mly"
                          ( ASTArgPAddress(_2,_4))
# 476 "parser.ml"
               : Ast.argp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.argp) in
    Obj.repr(
# 104 "parser.mly"
         ( [_1] )
# 483 "parser.ml"
               : Ast.argp list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.argp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.argp list) in
    Obj.repr(
# 105 "parser.mly"
                       ( _1::_3 )
# 491 "parser.ml"
               : Ast.argp list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 109 "parser.mly"
           ( ASTExpr(_1) )
# 498 "parser.ml"
               : Ast.exprp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 110 "parser.mly"
                          ( ASTExprAddress(_3) )
# 505 "parser.ml"
               : Ast.exprp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprp) in
    Obj.repr(
# 113 "parser.mly"
            ( [_1] )
# 512 "parser.ml"
               : Ast.exprp list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.exprp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprp list) in
    Obj.repr(
# 114 "parser.mly"
                   ( _1::_2 )
# 520 "parser.ml"
               : Ast.exprp list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 119 "parser.mly"
        ( ASTNum(_1))
# 527 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 120 "parser.mly"
          ( ASTId(_1) )
# 534 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 121 "parser.mly"
                                ( ASTIf(_3, _4, _5) )
# 543 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 122 "parser.mly"
                            ( ASTAnd(_3, _4) )
# 551 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 123 "parser.mly"
                           ( ASTOr(_3, _4) )
# 559 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 124 "parser.mly"
                         ( ASTApp(_2, _3) )
# 567 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 125 "parser.mly"
                        ( ASTLambda(_2,_4) )
# 575 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 126 "parser.mly"
                         ( ASTAlloc(_3))
# 582 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 127 "parser.mly"
                       ( ASTLen(_3))
# 589 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 128 "parser.mly"
                            (ASTNth(_3,_4))
# 597 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 129 "parser.mly"
                                  (ASTVset(_3,_4,_5))
# 606 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 133 "parser.mly"
         ( [_1] )
# 613 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 134 "parser.mly"
               ( _1::_2 )
# 621 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 138 "parser.mly"
          ( ASTLval(_1))
# 628 "parser.ml"
               : Ast.lval))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.lval) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 139 "parser.mly"
                              ( ASTNthLval(_3,_4))
# 636 "parser.ml"
               : Ast.lval))
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
