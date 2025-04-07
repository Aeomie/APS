open Ast

module StringMap = Map.Make(String)
type value = 
  | InZ of int
  | InF of expr * string list * environment (* Here p is expr *)
  | InFR of expr * string * string list * environment (* Here p is expr *)
  | InPrim of string (* Contains the name of the function *)
  | InAddress of address
  | InP of block * string list *  environment
  | InPR of block * string * string list * environment
  | None
and environment = value StringMap.t
and address = InA of int;;

(* AddressMap definition*)
module AddressMap = Map.Make(struct
  type t = address
  let compare = compare
end
)
module PointerMap = Map.Make(String)

type memMapT = value AddressMap.t

let memory : memMapT = AddressMap.empty
let env : environment = StringMap.empty

let global_ptr_cpt = ref 0

let list = ["not"; "eq"; "lt"; "add"; "sub"; "mul"; "div"]

let rec init env list = 
  match list with 
  | [] -> env  (*return the final env*)
  | hd::t -> 
    let new_env = StringMap.add hd (InPrim hd) env in  (* Add to environment *)
    init new_env t 

(* initialising environment*)
let env = init env list

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

let get_val_from_memory address mem =
  match AddressMap.find_opt address mem with
    | Some res -> res
    | None -> failwith "Value doesnt exist in memory" 

let alloc mem = 
  let allocation = InA(!global_ptr_cpt) in 
  let memory = AddressMap.add (allocation) (None) mem in
  global_ptr_cpt := !global_ptr_cpt + 1;
  (allocation,memory)

let add_val_to_memory v address mem =
  let new_mem = AddressMap.add address (v) mem in
  new_mem

let get_bool_val v =
  match v with
  | InZ 0 -> false
  | InZ 1 -> true
  | _ -> failwith "Not a boolean val"

let rec add_variables_to_env (args : string list) (values : value list) ( env: environment)=
let rec aux_add args vals res_env = 
  match args,vals with
  | [], [] -> res_env
  | id :: rest_args , value::rest_values -> 
    let res = StringMap.add id value res_env in
    aux_add rest_args rest_values res
  | _ -> failwith "Arguments mismatch in function"
  in
  aux_add args values env


let args_list_tostring arglist = 
  let rec aux_transform arg_list res = 
    match arg_list with
    | [] -> List.rev res
    | ASTArg(ident,_)::td -> aux_transform td (ident::res)
  in 
  aux_transform arglist [];; 

let print_int_value value =
  match value with
  | InZ(n) -> Printf.printf "%d\n" n
  | _ -> failwith "Can't print non-integer type"

let print_address_value value mem = 
    match AddressMap.find_opt value mem with
    | Some res -> print_int_value res
    | None -> failwith "Address doesn't exist"

let print_val value mem =
  match value with
  | InAddress a -> print_address_value a mem
  | _ -> print_int_value value
  
let print_output lst mem = 
  List.rev lst |> List.iter (fun x -> print_val x mem)
  

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

(* expression evaluator*)
(* expression evaluator*)
let rec eval_expr e env memory = 
  match e with
  | ASTId("true")-> InZ(1)
  | ASTId("false") -> InZ(0)
  | ASTNum n -> InZ n 
  | ASTId s -> 
    let value = (get_val s env) in
    (match value with
    | InAddress a -> get_val_from_memory a memory
    | v -> v
    )
  | ASTAnd(expr1,expr2) ->
     if(eval_expr expr1 env memory) = InZ 0 
     then InZ 0
     else eval_expr expr2 env memory
  | ASTOr(expr1,expr2) ->
    if(eval_expr expr1 env memory) = InZ 1
      then InZ 1
    else eval_expr expr2 env memory
  | ASTIf(condition,body,alternate)->
    let value = eval_expr condition env memory in
    let cond = get_bool_val value in
    if(cond) then
      eval_expr body env memory
    else
      eval_expr alternate env memory
  | ASTApp(expr , exprs) -> eval_app expr exprs env memory
  | ASTLambda(args, e) ->
    let new_args = args_list_tostring args in
    eval_lambda new_args e env

and eval_exprs es env memory = 
  match es with 
  | [] -> []
  | e::exprs -> 
    let value = eval_expr e env memory in
    value::eval_exprs exprs env memory

and eval_app expr expressions env memory =
  let new_v = eval_expr expr env memory in 
  let arg_values = eval_exprs expressions env memory in 
  match new_v with
  | InPrim id ->
    if List.length expressions > 2 then
      failwith "Not the right args for the inP, no more than 2"
    else
      wrap id arg_values env  (* Reversing the list to maintain original order *)

  | InF (body, params, env_fun) ->
      let new_env = add_variables_to_env params arg_values env_fun in
      eval_expr body new_env memory 
  | InFR (body, ident, params, env_fun) -> 
        let rec_env = StringMap.add ident (InFR (body, ident, params, env)) env_fun in
        let new_env = add_variables_to_env params arg_values rec_env in
        eval_expr body new_env memory
  | _ -> failwith"Not an existing app"

and eval_lambda params expr env =
  InF(expr, params, env)

and eval_def def env memory =
  match def with
  | ASTConst (ident, ttype, expr) ->
      let new_val = eval_expr expr env memory in
      let new_env = StringMap.add ident new_val env in  (* Correctly update env *)
      (new_env ,memory)

  | ASTFun (ident, ttype, arg_list, expr) ->
      let new_args = args_list_tostring arg_list in 
      let new_val = InF(expr, new_args, env) in
      let new_env = StringMap.add ident new_val env in 
      (new_env, memory)
  
  | ASTFunRec (ident, ttype, arg_list, expr) ->
    let new_args = args_list_tostring arg_list in
    let new_val = InFR (expr, ident, new_args, env) in
    let new_env = StringMap.add ident new_val env in
    (new_env,memory)

  | ASTVar (ident, ttype) -> 
    let (address,new_mem) = alloc memory in
    let new_env = StringMap.add ident (InAddress address) env in
    (new_env,new_mem)

  | ASTProc(ident,arg_list, block) ->
    let new_args = args_list_tostring arg_list in
    let new_val = InP(block, new_args, env) in
    let new_env = StringMap.add ident new_val env in
    (new_env,memory)

  | ASTProcRec(ident,arg_list,block) ->
    let new_args = args_list_tostring arg_list in
    let new_val = InPR(block, ident, new_args , env) in
    let new_env = StringMap.add ident new_val env in 
    (new_env,memory)

and eval_stat s env memory output = 
  match s with 
  | ASTEcho e -> 
      let new_val = eval_expr e env memory in 
      (memory, new_val :: output)
  | ASTSet(var, expr) ->
      let new_val = eval_expr expr env memory in
      let address = get_val var env in
      (match address with
      | InAddress ad ->
          let new_mem = add_val_to_memory new_val ad memory in
          (new_mem, output)
      | _ -> failwith "Not a valid variable")

  | ASTIfB(condition, body, alt) ->
    let value = eval_expr condition env memory in
    let cond = get_bool_val value in
    if(cond)then
      let (new_mem, new_output) = eval_block body env memory output in
      (new_mem, new_output)
    else
      let (new_mem, new_output) = eval_block alt env memory output in
      (new_mem, new_output)

  | ASTWhile(condition , body) ->
    let value = eval_expr condition env memory in
    let cond = get_bool_val value in
    if (not cond) then
      (memory, output)
    else
      let (new_mem, new_output) = eval_block body env memory output in
      let (final_mem , final_output) = eval_stat s env new_mem new_output in
      (final_mem, final_output)
  
  | ASTCall (ident, exprs) -> 
    let arg_values = eval_exprs exprs env memory in
    let proc = get_val ident env in
   (match proc with
    | InP(body,params,proc_env) ->
      let new_env = add_variables_to_env params arg_values proc_env in
      let (new_mem, new_output) = eval_block body new_env memory output in
      (new_mem, new_output)
    
    | InPR(body, name, params, proc_env) ->
      let rec_env = StringMap.add name (InPR(body,name,params,proc_env)) proc_env in
      let new_env = add_variables_to_env params arg_values rec_env in
      let (new_mem, new_output) =  eval_block body new_env memory output in
      (new_mem, new_output)
    | _ -> failwith "Expected procedure but got something else")

and eval_cmd cmd env memory output = 
  match cmd with
  | ASTStat stat -> 
    let (res_mem, res_output) = eval_stat stat env memory output in
    (res_mem, res_output)

  | ASTDef (def, cmds) -> 
    let (new_env, new_mem) = eval_def def env memory in
    eval_cmd cmds new_env new_mem output

  | ASTStatcmds(stat, cmds) ->
    let (res_mem, res_output) = eval_stat stat env memory output in 
    let (final_mem, final_output) = eval_cmd cmds env res_mem res_output in
    (final_mem, final_output)

and eval_block b env memory output = 
  match b with
  | ASTBlock cmd -> eval_cmd cmd env memory output

and eval_prog block env mem = 
  let(res_mem, output) = eval_block block env mem [] in 
  print_output output res_mem
;;

let fname = Sys.argv.(1) in
  let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      eval_prog p env memory;
  with Lexer.Eof ->
    exit 0

  