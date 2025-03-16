open Ast

module StringMap = Map.Make(String)

type value = 
  | InZ of int
  | InF of expr * string list * env(* here should be p , but idk whats p yet*) (* string list because of the variables*)
  | InFR of expr * string * string list * env (* here p too*) 
  | InP of string (* contains the name of the function *)

(* following this principal , tommorow add not lt mul div eq just like shown in the cours*)

type environment = value StringMap.t

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

(* to get the value from the base environment*)
let get_val ident env =
  match StringMap.find_opt ident env with
  | Some v -> v
  | None -> failwith ("No value for identifier: " ^ ident)


(* expression evaluator*)
let rec eval_expr e env = 
  match e with
  | ASTNum n -> InZ n
  | ASTId s -> get_val s
  | ASTand (e1,e2) -> wrap and_ e1 e2 env
  | ASTOr (e1,e2) -> wrap or_ e1 e2 env
  | ASTif (e1,e2,e3) -> wrap if_ e1 e2 e3 env
  (* defs here just match them then call it on their own func*)
  | ASTConst (ident, ttype , expr) -> eval_def e env
  | ASTFun(ident, ttype, arg_list, expr) -> eval_def e env
  | ASTFunRec(ident, ttype, arg_list, expr) -> eval_def e env
  (* end of defs*)
  | ASTLambda (args,e) -> eval_lambda args e env
  | ASTApp (expr, exprs) -> eval_app expr exprs env
  | _ ->  failwith " Unknown expression"



(* All funcs of defs return a new env *)
(* here im thinking of pushing env as a parameter*)
let eval_def def env =
  match def with
  | ASTConst (ident, ttype, expr) ->
    let new_env = StringMap.add ident ( eval_expr expr ) env in
    new_env 

  | ASTFun (ident, ttype, arg_list, expr) ->
    let new_env = StringMap.add ident(InF (expr,arg_list,env)) env in 
    new_env
  
  | ASTFunRec (ident, ttype, arg_list, expr) ->
    let new_env = StringMap.add ident (InFR (expr, ident, arg_list, env)) env in
    new_env

  | _ -> failwith "no such definition"
 

(* init functions *)
let not_op_func arg =
  match arg with
  | InZ 0 -> inZ 1 
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

(* ajout des fonctions à l'environnement *)

(* functions for evaluations *)
let and_func args env =
  match args with
  | [expr1 ; expr2] ->
    if (eval_expr expr1 env) = InZ 0 then 
      InZ 0
    else
      eval_expr expr2 env
  | _ -> failwith "Invalid arguments for and operation"

let or_func args env =
  match args with
  | [expr1; expr2] ->
    if (eval_expr expr1 env) = Inz 1 then 
      Inz(1)
    else
      eval_expr expr2
  | _ -> failwith "Invalid arguments for or operation"

let if_func args env =
  match args with
  | [condition ; body ; alternate] ->
    if (eval_expr condition env) = InZ 1 then 
      eval_expr body env
    else
      eval_expr alternate env
  | _ -> failwith "Invalid arguments for if operation"

(* we get the function f
  We check its parameters , if they are correct 
  it'll get the values from its env
*)
let eval_app f args env =
  match (get_val f env) with
  | InP id ->
    if List.length args > 2 then
      failwith "Not the right args for the inP, no more than 2"
    else
      let rec aux_prim args new_args =
        match args with
        | [] -> new_args
        | hd :: t -> aux_prim t (eval_expr hd :: new_args)  (* Accumulating in reverse order *)
      in
      let new_args = aux_prim args [] in
      prim id (List.rev new_args) env  (* Reversing the list to maintain original order *)


  | InF (exp, params, env) ->
      if checkArgs params args then 
        let new_env = add_args args env in
        eval_expr exp new_env 
      else
        failwith ("Argument type mismatch for " ^ f)

  | InFR (exp, ident, params, env) -> 
      if checkArgs params args then
        let rec_env = StringMap.add ident (InFR (exp, ident, params, env)) env in
        let new_env = add_args args rec_env in
        eval_expr exp new_env
      else
        failwith "Argument type mismatch"
      
  | _ -> failwith "App call doesn't exist"



let eval_lambda params expr env=
    InF(expr, params, env)

  (* the checks for inZ are inside the functions *)

let prim ident args = 
  match args with
  | [arg] -> 
    prim1 arg
  | [arg1;arg2] ->
    prim2 ident arg1 arg2
  | _ -> failwith " not a prim function"

let prim1 arg = not_op arg;
let prim2 ident arg1 arg2 = 
  match ident with
  | eq -> eq_func [arg1::arg2]
  | lt -> lt_func [arg1::arg2]
  | add -> add_func [arg1::arg2]
  | sub -> sub_func [arg1::arg2]
  | mul -> mul_func [arg1::arg2]
  | div -> div_func [arg1::arg2]
  | _ -> failwith "Invalid function name"


let wrap ident arg1 arg2 env = 
  match ident with
  | and_ -> and_func [arg1::arg2] env
  | or_ -> or_func [arg1::arg2] env
  | _ -> failwith "incorrect call to wrap 1"

(* wrapper for 3 parameters *)
let wrap2 ident arg1 arg2 arg3 env =
  match ident with
  | if_ -> if_func [arg1::arg2::arg3] env
  | _ -> failwith "incorrect call to wrap 2 "


let rec checkArgs args params = 
  if List.length args <> List.length params then
    failwith "Argument length mismatch"
  else
    let rec aux args params = 
      match (args, params) with
      | ([], []) -> true  (* Both lists are empty, so types match *)
      | InZ _ :: rest1, InZ _ :: rest2 -> aux rest1 rest2  (* Both are InZ, check the rest *)
      | InF _ :: rest1, InF _ :: rest2 -> aux rest1 rest2  (* Both are InF, check the rest *)
      | InFR _ :: rest1, InFR _ :: rest2 -> aux rest1 rest2  (* Both are InFR, check the rest *)
      | _ -> false 
    in 
    aux args param;;

let rec add_args args env =
  match args with
  | [] -> env
  | arg::rest ->
    let new_env = eval_expr arg env
  in
  add_args rest new_env
  
let rec num_of_int n = 
  int_of_string n


let rec check_expr e =
  match (StringMap.find_opt e env) with
  |Some _ -> true
  |None -> false


let rec eval_ttype typ =
match typ with
|ASTBool b -> 
  if b = true then InZ 1
  else InZ 0
|ASTInt i-> intZ(num_of_int n)
|ASTArrow list ->
  match list with
  |[] -> 
  |t::rest -> (eval_ttype t) :: (eval_ttype rest)