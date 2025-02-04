
open Ast



let rec print_type t =
  match t with
  | ASTBool -> Printf.printf" ASTBool "
  | ASTInt -> Printf.printf" ASTInt "
  | ASTArrow (ts,t2) -> (
    Printf.printf"ASTArrow(";
    print_types ts;
    print_char ',';
    print_type t2;
    Printf.printf ")";
  )

and print_types ts = 
  match ts with
  | [] -> ()
  | [t] -> print_type t
  | t::ts -> (
    print_type t;
    print_char ',';
    print_types ts;
    )


let print_arg a = 
  match a with
  | ASTArg(str, t) -> (
        Printf.printf "ASTArg(%s,"str;
        print_type t;
        Printf.printf")";
      )
  
  let rec print_args args = 
  match args with
  | [] -> ()
  | [a] -> print_arg a
  | a::args -> (
    print_arg a;
    print_char ',';
    print_args args;
  )
      
    
let rec print_expr e =
  match e with
  | ASTNum n -> Printf.printf"ASTNum(%d)" n
  | ASTId x -> Printf.printf"ASTId(%s)" x
  | ASTApp(e, es) -> (
      Printf.printf"ASTApp(";
      print_expr e;
      Printf.printf",[";
      print_exprs es;
      Printf.printf"])";
  )
  | ASTAnd(e, ne) -> (
      Printf.printf"ASTAnd(";
      print_expr e;
      print_char ',';
      print_expr ne;
      Printf.printf")";
  )
  | ASTOr(e, ne) -> (
      Printf.printf"ASTOr(";
      print_expr e;
      print_char ',';
      print_expr ne;
      Printf.printf")";
  )
  | ASTIf(con, body, alternant) -> (
      Printf.printf"ASTIf(";
      print_expr con;
      print_char ',';
      print_expr body;
      print_char ',';
      print_expr alternant;
      Printf.printf")";
  )
  | ASTLambda(args, e) -> (
      Printf.printf"ASTLambda(";
      Printf.printf"[";
      print_args args;
      Printf.printf"],";
      print_expr e;
      Printf.printf")";
  )
 and print_exprs es =
  match es with
    | [] -> ()
    | [e] -> print_expr e 
    | e::es -> (
      print_expr e;
      print_char ',';
      print_exprs es;
    )


let print_stat s = 
  match s with
  | ASTEcho e -> (
      Printf.printf"ASTEcho(";
      print_expr e;
      Printf.printf")";
  )

let print_def d = 
  match d with
  | ASTConst(str , ttype, expr) -> (
      Printf.printf"ASTConst( %s," str;
      print_type ttype;
      print_char ',';
      print_expr expr;
      Printf.printf ")" 
  )
  | ASTFun(str, ttype, args, expr) -> (
      Printf.printf"ASTFun(%s,"str;
      print_type ttype;
      print_char ',';
      print_args args;
      print_char ',';
      print_expr expr;
      Printf.printf")";
  )
  | ASTFunRec(str, ttype, args, expr) -> (
      Printf.printf"ASTFunRec(%s,"str;
      print_type ttype;
      print_char ',';
      print_args args;
      print_char ',';
      print_expr expr;
      Printf.printf")";
  )

let print_cmd c = 
  match c with
  | ASTStat s -> print_stat s
  | ASTDef d -> print_def d

let rec print_cmds cs = 
  match cs with
  | [] -> ()
  | [c] -> print_cmd c
  | c::cs -> (
    print_cmd c;
    print_char ',';
    print_cmds cs;
  )


let print_prog p =
  Printf.printf"ASTProg([";
  print_cmds p;
  Printf.printf"])"
;;

let fname = Sys.argv.(1) in
let ic = open_in fname in
try
  let lexbuf = Lexing.from_channel ic in
  let p = Parser.prog Lexer.token lexbuf in
    print_prog p;
    print_string ".\n"
with Lexer.Eof ->
  exit 0
      