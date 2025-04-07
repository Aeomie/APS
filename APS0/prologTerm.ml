
open Ast



let rec print_type t =
  match t with
  | ASTBool -> Printf.printf"bool"
  | ASTInt -> Printf.printf"int"
  | ASTArrow (typs,typ) -> (
    Printf.printf("fun(");
    print_char '[';
    print_types typs;
    print_char ']';
    print_char ',';
    print_type typ;
    print_char ')';
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
  | ASTArg(name, t) -> (
        Printf.printf "(%s,"name;
        print_type t;
        Printf.printf")";
      )
  
  let rec print_args args = 
  match args with
  | [] -> ()
  | [a] -> print_arg a
  | a::restargs -> (
    print_arg a;
    print_char ',';
    print_args restargs;
  )
      
    
let rec print_expr e =
  match e with
  | ASTNum n -> Printf.printf"num(%d)" n
  | ASTId x -> Printf.printf"id(%s)" x
  | ASTIf(con, body, alternant) -> (
    Printf.printf"if(";
    print_expr con;
    print_char ',';
    print_expr body;
    print_char ',';
    print_expr alternant;
    Printf.printf")";
    )
  | ASTAnd(e1, e2) -> (
      Printf.printf"and(";
      print_expr e1;
      print_char ',';
      print_expr e2;
      Printf.printf")";
      )
  | ASTOr(e1, e2) -> (
        Printf.printf"or(";
        print_expr e1;
        print_char ',';
        print_expr e2;
        Printf.printf")";
    )
  | ASTApp(e, es) -> (
      Printf.printf"app(";
      print_expr e;
      Printf.printf",[";
      print_exprs es;
      Printf.printf"])";
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
      print_char ',';
      print_exprs es;
    )


let print_stat s = 
  match s with
  | ASTEcho e -> (
      Printf.printf"echo(";
      print_expr e;
      Printf.printf")";
  )

let print_def d = 
  match d with
  | ASTConst(name , ttype, expr) -> (
      Printf.printf"const( %s," name;
      print_type ttype;
      print_char ',';
      print_expr expr;
      Printf.printf ")" 
  )
  | ASTFun(name, ttype, args, expr) -> (
      Printf.printf"fun(%s,"name;
      print_type ttype;
      print_char ',';
      print_char '[';
      print_args args;
      print_char ']';
      print_char ',';
      print_expr expr;
      Printf.printf")";
  )
  | ASTFunRec(name, ttype, args, expr) -> (
      Printf.printf"funRec(%s,"name;
      print_type ttype;
      print_char ',';
      print_char '[';
      print_args args;
      print_char ']';
      print_char ',';
      print_expr expr;
      Printf.printf")";
  )

let rec print_cmd c = 
  match c with
  | ASTStat s -> print_stat s
  | ASTDef(d,c) -> (
    Printf.printf "dec(";
    print_def d;
    print_char ')';
    print_char ',';
    print_cmd c;
    )


let print_prog p =
  Printf.printf"prog([";
  print_cmd p;
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
      