
open Ast

let rec print_expr e =
  match e with
  | ASTNum n -> Printf.printf"num(%d)" n
  | ASTId x -> Printf.printf"id(%s)" x
  | ASTApp(e, es) -> (
      Printf.printf"app(";
      print_expr e;
      Printf.printf",[";
      print_exprs es;
      Printf.printf"])";
  )
  | ASTAnd(e, ne) -> (
      Printf.printf"and(";
      print_expr e;
      print_char ',';
      print_expr ne;
      Printf.printf")";
  )
  | ASTOr(e, ne) -> (
      Printf.printf"or(";
      print_expr e;
      print_char ',';
      print_expr ne;
      Printf.printf")";
  )
  | ASTIf(con, body, alternant) -> (
      Printf.printf"if(";
      print_expr con;
      print_char ',';
      print_expr body;
      print_char ',';
      print_expr alternant;
      Printf.printf")";
  )
  | ASTLambda(args, e) -> (
      Printf.printf"lambda(";
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
      print_char ",";
      print_exprs es;
    )

let print_cmd c = 
  match c with
  | ASTstat s -> print_stat s
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

let print_stat s = 
  match s with
  | ASTEcho e -> (
      Printf.printf"echo (";
      print_expr e;
      Printf.printf")";
  )

let print_def d = 
  match d with
  | ASTConst(str , ttype, expr) -> (
      Printf.printf"const( %s," str;
      print_type ttype;
      print_char ',';
      print_expr expr;
      Printf.printf ")" 
  )
  | ASTFun(str, ttype, args, expr) -> (
      Printf.printf"fun(%s,"str;
      print_type ttype;
      print_char ',';
      print_args args;
      print_char ',';
      print_expr expr;
      Printf.printf")";
  )
  | ASTFunRec(str, ttype, args, expr) -> (
      Printf.printf"funRec(%s,"str;
      print_type ttype;
      print_char ',';
      print_args args;
      print_char ',';
      print_expr expr;
      Printf.printf")";
  )

let rec print_type t =
  match t with
  | ASTBool -> Printf.printf"bool()"
  | ASTInt -> Printf.printf"int()"
  | ASTArrow (ts,t2) -> (
    Printf.printf"arrow(";
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
  | ASTarg(str, t) -> (
        Printf.printf "arg(%s,"str;
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

let print_prog p =
  Printf.printf"prog([";
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
      