open Ast

module StringMap = Map.Make(String)

type value = 
  | InZ of int
  | InF of expr * string list * environment (* Here p is expr *)
  | InFR of expr * string * string list * environment (* Here p is expr *)
  | InP of string (* Contains the name of the function *)
and environment = value StringMap.t;;

(* following this principal , tommorow add not lt mul div eq just like shown in the cours*)


let env : environment = StringMap.empty

let list = ["not"; "eq"; "lt"; "add"; "sub"; "mul"; "div"]

let rec init env list = 
  match list with 
  | [] -> env  (*return the final env*)
  | hd::t -> 
    let new_env = StringMap.add hd (InP hd) env in  (* Add to environment *)
    init new_env t 

(* initialising environment*)
let env = init env list

(*
General rules :
InZ 1 , also plays the role of true.
InZ 0 plays the role of false

*)


(* init functions *)
let not_op_func arg =
  match arg with
  | InZ 0 -> InZ 1 
  | InZ _ -> InZ 0
  | _ -> assert false

let eq_func args = 
  match args with
  | [InZ(a); InZ(b)] -> if a = b then InZ(1) else InZ(0)
  | _ -> assert false 

let lt_func args = 
  match args with
  | [InZ(a); InZ(b)] -> if a < b then InZ(1) else InZ(0)
  | _ -> assert false 

let add_func args = 
  match args with
  | [InZ(a); InZ(b)] -> InZ(a + b)
  | _ -> assert false 

let sub_func args =
  match args with
  | [InZ(a); InZ(b)] -> InZ(a - b)
  | _ -> assert false

let mul_func args =
  match args with
  | [InZ(a); InZ(b)] -> InZ(a * b)
  | _ -> assert false
    
let div_func args =
  match args with
  | [InZ(a); InZ(b)] when b <> 0 -> InZ(a / b)
  | _ -> assert false 
(* to get the value from the base environment*)
let get_val ident env =
  match StringMap.find_opt ident env with
  | Some v -> v
  | None -> failwith ("No value for identifier: " ^ ident)
;;



let rec add_variables_to_env (args : string list) (values : value list) ( env: environment)=
  let rec aux_add args vals res_env = 
    match args,vals with
    | [], [] -> res_env
    | id :: rest_args , value::rest_values -> 
      let res = StringMap.add id value res_env in
      aux_add rest_args rest_values res
    | _ -> failwith "Arguments mismatch in function"
    in
    aux_add args values env;;

let prim1 arg = not_op_func arg;;
let prim2 ident arg1 arg2 env = 
  match ident with
  | "eq" -> eq_func [arg1;arg2]
  | "lt" -> lt_func [arg1;arg2]
  | "add" -> add_func [arg1;arg2]
  | "sub" -> sub_func [arg1;arg2]
  | "mul" -> mul_func [arg1;arg2]
  | "div" -> div_func [arg1;arg2]
  | _ -> failwith "Invalid function name"
;;

let wrap ident args env = 
  match args with
  | [arg] -> 
    prim1 arg
  | [arg1;arg2] ->
    prim2 ident arg1 arg2 env
  | _ -> failwith " not a prim function";;

  let find_key_for_value map value =
    StringMap.fold (fun key v result -> 
      if v = value && result = None then Some key else result
    ) map None
    ;;

let eval_lambda params expr env =
  InF(expr, params, env);;

let args_list_tostring arglist = 
  let rec aux_transform arg_list res = 
    match arg_list with
    | [] -> List.rev res
    | ASTArg(ident,_)::td -> aux_transform td (ident::res)
  in 
  aux_transform arglist [];; 
(* expression evaluator*)
let rec eval_expr e env = 
  match e with
  | ASTId("true")-> InZ(1)
  | ASTId("false") -> InZ(0)
  | ASTNum n -> InZ n
  | ASTId s -> get_val s env
  | ASTAnd(expr1,expr2) ->
     if(eval_expr expr1 env) = InZ 0 
     then InZ 0
     else eval_expr expr2 env
  | ASTOr(expr1,expr2) ->
    if(eval_expr expr1 env) = InZ 1
      then InZ 1
    else eval_expr expr2 env
  | ASTIf(condition,body,alternate)->
    if(eval_expr condition env) = InZ 1 then
      eval_expr body env
    else
      eval_expr alternate env
  | ASTApp(expr , exprs) -> eval_app expr exprs env
  | ASTLambda(args, e) ->
    let new_args = args_list_tostring args in
    eval_lambda new_args e env
  | _ ->  failwith " Unknown expression"

and eval_exprs es env = 
  match es with 
  | [] -> []
  | e::exprs -> (eval_expr e env)::eval_exprs exprs env

and eval_app expr expressions env =
  let new_v = eval_expr expr env in 
  let arg_values = eval_exprs expressions env in 
  match new_v with
  | InP id ->
    if List.length expressions > 2 then
      failwith "Not the right args for the inP, no more than 2"
    else
      wrap id arg_values env  (* Reversing the list to maintain original order *)

  | InF (body, params, env_fun) ->
      let new_env = add_variables_to_env params arg_values env_fun in
      eval_expr body new_env 
  | InFR (body, ident, params, env_fun) -> 
        let rec_env = StringMap.add ident (InFR (body, ident, params, env)) env_fun in
        let new_env = add_variables_to_env params arg_values rec_env in
        eval_expr body new_env
  | _ -> failwith"Not an existing app"
  ;;


let eval_def def env =
  match def with
  | ASTConst (ident, ttype, expr) ->
      let new_env = StringMap.add ident (eval_expr expr env) env in  (* Correctly update env *)
      new_env 

  | ASTFun (ident, ttype, arg_list, expr) ->
      let new_args = args_list_tostring arg_list in 
      let new_val = InF(expr, new_args, env) in
      let new_env = StringMap.add ident new_val env in 
      new_env
  
  | ASTFunRec (ident, ttype, arg_list, expr) ->
    let new_args = args_list_tostring arg_list in 
    let new_env = StringMap.add ident (InFR (expr, ident, new_args, env)) env in
    new_env
  ;;





let print_value value =
  match value with
    InZ(n) -> Printf.printf "%d\n" n
  | _ -> failwith "Can't print non integer type"

let print_output lst = 
  List.iter (function x -> print_value x) (List.rev lst) 
;;



let eval_stat s env output = 
  match s with 
  | ASTEcho e -> (eval_expr e env )::output
;;

let rec eval_cmd cmd env output = 
  match cmd with
  | ASTStat stat -> eval_stat stat env output
  | ASTDef (def, cmds) -> 
    let new_env = eval_def def env in
    eval_cmd cmds new_env output
;;

let eval_prog p env = 
  print_output (eval_cmd p env [] )
;;

let fname = Sys.argv.(1) in
  let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      eval_prog p env;
  with Lexer.Eof ->
    exit 0
        