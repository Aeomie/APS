open Ast

module StringMap = Map.Make(String)
type value = 
  | InZ of int
  | InF of expr * string list * environment (* Here p is expr *)
  | InFR of expr * string * string list * environment (* Here p is expr *)
  | InPrim of string (* Contains the name of the function *)
  | InAddress of address
  | InAdBlock of bloc (* Address Block *)
  | InP of block * string list *  environment
  | InPR of block * string * string list * environment
  | None
and environment = value StringMap.t
and address = InA of int
and bloc = InB of address * int;;

(* AddressMap definition*)
module AddressMap = Map.Make(struct
  type t = address
  let compare = compare
end
)

type memMapT = value AddressMap.t

let memory : memMapT = AddressMap.empty
let env : environment = StringMap.empty

let global_ptr_cpt = ref 0

let list = ["not"; "eq"; "lt"; "add"; "sub"; "mul"; "div"]

(*********************************)
(*********************************)
(**********HELPER FUNCTIONS*******)


let get_val_from_env ident env =
  match StringMap.find_opt ident env with
  | Some v -> v
  | None -> failwith ("No value for identifier: " ^ ident)

let add_val_to_env ident value env = 
  let new_env = StringMap.add ident value env in 
  new_env;;

let get_val_from_memory address mem =
  match AddressMap.find_opt address mem with
    | Some res -> res
    | None -> failwith "Value doesnt exist in memory" ;;

let add_val_to_memory v address mem =
  let new_mem = AddressMap.add address (v) mem in
  new_mem;;


(* initialises environment*)
let rec init env list = 
  match list with 
  | [] -> env  (*return the final env*)
  | hd::t -> 
    let new_env = add_val_to_env hd (InPrim hd) env in  (* Add to environment *)
    init new_env t 

(* alloc memory*)
let alloc mem = 
  let allocation = InA(!global_ptr_cpt) in 
  let memory = AddressMap.add (allocation) (InZ 0) mem in
  global_ptr_cpt := !global_ptr_cpt + 1;
  (allocation,memory);;

(*APS 2 Helper Functions*)

(* alloc N memory*)
let allocn n mem =
  if n <= 0 then 
    failwith "allocn only takes values greater than 0"
  else
    let address = InA(!global_ptr_cpt) in
    let rec aux_allocn cpt memory =
      match cpt with
      | 0 -> memory
      | _ ->
        let (allocation, new_mem) = alloc memory in
        aux_allocn (cpt - 1) new_mem
    in
    let final_mem = aux_allocn n mem in 
    (address, final_mem);;

(* used when trying to get the size from a bloc or the address*)
let get_adblock_val value = 
  match value with
  | InAdBlock blk -> 
    (match blk with 
      | InB (address, size) -> (address, size)
      | _ -> failwith "Vector size not well initialized"
    )
  | _ -> failwith "Value is not an AdBlock";;

let get_mem_Address address = 
  match address with
  | InA a -> a
  | _ -> failwith "Val passed is not an address";;

let get_Vec_ncase n address = 
  let address_val = get_mem_Address address in
  let res_address = InA(address_val + n ) in (* pointing towards the right address*) 
  (res_address);;


(* End of APS 2 Helper Functions *)

let get_bool_val v =
  match v with
  | InZ 0 -> false
  | InZ 1 -> true
  | _ -> failwith "Not a boolean val"

let int_from_Val v = 
  match v with 
  | InZ(n) -> n
  | _ -> failwith "Val passed not InZ"

let rec add_variables_to_env (args : string list) (values : value list) ( env: environment)=
  let rec aux_add args vals res_env = 
    match args,vals with
    | [], [] -> res_env
    | id :: rest_args , value::rest_values -> 
      let res = add_val_to_env id value res_env in
      aux_add rest_args rest_values res
    | _ -> failwith "Arguments mismatch in function"
    in
    aux_add args values env


(* Arg functions*)
let args_list_tostring arglist = 
  let rec aux_transform arg_list res = 
    match arg_list with
    | [] -> List.rev res
    | ASTArg(ident,_)::td -> aux_transform td (ident::res)
  in 
  aux_transform arglist [];; 

let argsp_list_tostring argplist = 
  let rec aux_transform argp_list res =
    match argp_list with
    | [] -> List.rev res
    | ASTArgP(ident,_)::td -> aux_transform td (ident::res)
    | ASTArgPAddress(ident,_)::td -> aux_transform td (ident::res)
  in
  aux_transform argplist [];;


(* Print functions *)
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

(******END HELPER FUNCTIONS*******)
(*********************************)
(*********************************)


(* rest is just logic of the AST*)

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
let rec eval_expr e env memory = 
  match e with
  | ASTId("true")-> (InZ(1),memory)
  | ASTId("false") -> (InZ(0),memory)
  | ASTNum n -> (InZ(n),memory)
  | ASTId s -> 
    let value = (get_val_from_env s env) in
    (match value with
    | InAddress a ->
      let value = get_val_from_memory a memory in
      (value,memory)
    | v -> (v,memory)
    )

  | ASTAnd(expr1,expr2) ->
    let (left_value,new_mem) = eval_expr expr1 env memory in 
    let left_res = get_bool_val left_value in
     if (not left_res)
     then (InZ 0, new_mem)
     else 
      let (right_value, final_mem) = eval_expr expr2 env new_mem in
      (right_value, final_mem)

  | ASTOr(expr1,expr2) ->
    let (left_value , new_mem) = eval_expr expr1 env memory in
    let left_res = get_bool_val left_value in
    if(left_res)
      then (InZ 1, new_mem)
    else
      let (right_value , final_mem) = eval_expr expr2 env new_mem in
      (right_value , final_mem)

  | ASTIf(condition,body,alternate)->
    let (value, new_mem) = eval_expr condition env memory in
    let cond = get_bool_val value in
    if(cond) then
      let (body_val , final_mem) = eval_expr body env new_mem in
      (body_val , final_mem)
    else
      let (alt_val, final_mem) = eval_expr alternate env new_mem in
      (alt_val, final_mem)
    
  | ASTApp(expr , exprs) ->
     let (eval_val , new_mem) = eval_app expr exprs env memory in
     (eval_val, new_mem)

  | ASTLambda(args, e) ->
    let new_args = args_list_tostring args in
    let value = eval_lambda new_args e env in
    (value, memory)

  | ASTAlloc(e) -> 
    let (value , new_mem) = eval_expr e env memory in
    let int_val = int_from_Val value in
    let (address, final_mem) = allocn int_val new_mem in
    let addressBlock = InB(address, int_val) in
    let bloc = InAdBlock(addressBlock) in 
    (bloc, final_mem)

  | ASTLen(e) -> 
    let (value, new_mem) = eval_expr e env memory in
    let (address, size) = get_adblock_val value in
    (InZ(size), new_mem)

  | ASTNth(vec,index) ->
    let (vec_value, new_mem) = eval_expr vec env memory in
    let (index_val, final_mem) = eval_expr index env new_mem in
    let index_int = int_from_Val index_val in
    let (address, size) = get_adblock_val vec_value in
    if(index_int < 0 || index_int >= size) then
      failwith "index out of bound"
    else
      let pointed_address = get_Vec_ncase index_int address in
      let value = get_val_from_memory pointed_address final_mem in
      (value, final_mem)

  | ASTVset(vec, index, e3) -> 
    let (vec_value, new_mem) = eval_expr vec env memory in
    let (index_val, new_mem2) = eval_expr index env new_mem in
    let index_int = int_from_Val index_val in
    let (address, size) = get_adblock_val vec_value in
    (* check bounds before evaluating third expression*)
    (* we could've tested < 0 first but i kept it this way for readabilty*)
    if(index_int < 0 || index_int >= size) then
      failwith "index out of bound"
    else
      let (new_val, new_mem3) = eval_expr e3 env new_mem2 in
      let pointed_address = get_Vec_ncase index_int address in
      let final_mem = add_val_to_memory new_val pointed_address new_mem3 in
      (vec_value, final_mem) (* inB val of vec_value and the final memory*)

and eval_app expr expressions env memory =
  let (new_v, new_mem) = eval_expr expr env memory in (* func name *)
  let (arg_values, _mem) = eval_exprs expressions env new_mem in 
  match new_v with
  | InPrim id ->
    if List.length expressions > 2 then
      failwith "Not the right args for the inP, no more than 2"
    else
      let value = wrap id arg_values env  in (* Reversing the list to maintain original order *)
      (value,new_mem)
  | InF (body, params, env_fun) ->
      let new_env = add_variables_to_env params arg_values env_fun in
      let (fun_val , final_mem) = eval_expr body new_env new_mem in
      (fun_val , final_mem)
  | InFR (body, ident, params, env_fun) -> 
        let rec_env = add_val_to_env ident (InFR (body, ident, params, env)) env_fun in
        let new_env = add_variables_to_env params arg_values rec_env in
        let (funR_val , final_mem) = eval_expr body new_env new_mem in
        (funR_val , final_mem)
  | _ -> failwith"Not an existing app"

and eval_lambda params expr env =
  InF(expr, params, env)

and eval_lval lval env memory = 
  match lval with
  | ASTLval(ident) ->
    let value = get_val_from_env ident env in
    (
    match value with
    | InAddress ad -> (ad,memory)
    | _ -> failwith ("Expected an identifier in Lvalue but got : " ^ ident)
    )

    (* its written like this to enforce that a vec value is only readable inside Nthlval*)
    (* otherwise we could add the check to match value*)
  | ASTNthLval(lv, e) ->
    let (value , new_mem) = eval_expr e env memory in
    let index_int = int_from_Val value in
    match lv with
    | ASTLval(ident) ->
      let v = get_val_from_env ident env in
      let (address, size) = get_adblock_val v in
      if(index_int < 0 || index_int >= size) then
        failwith "index out of bound"
      else
        let res_address = get_Vec_ncase index_int address in
        (res_address, new_mem)
      (* vec imbriqué*)
    | ASTNthLval (_)-> (* whats inside doesnt matter since we'll evaluate lv*)
      let (ad, new_mem2) = eval_lval lv env new_mem in
      let interm_ad = get_val_from_memory ad new_mem2 in
      let (address , size) = get_adblock_val (interm_ad) in
      if(index_int < 0 || index_int >= size) then
        failwith "index out of bound"
      else
        let res_address = get_Vec_ncase index_int address in
        (res_address, new_mem2)

and eval_exprs es env memory = 
  match es with 
  | [] -> ([],memory)
  | e::exprs -> 
    let (value, new_mem) = eval_expr e env memory in
    let (values , final_mem) = eval_exprs exprs env new_mem in
    (value::values, final_mem)

and eval_exprp expr env memory = 
  match expr with
  | ASTExpr e -> 
    let (value, new_mem) = eval_expr e env memory in
    (value,new_mem)
  | ASTExprAddress ident -> 
    let value = get_val_from_env ident env in
    match value with
    | InAddress a ->
       (InAddress a, memory)
    | _ -> failwith ("ident isn't of Address value, ident: " ^ ident)
    

and eval_exprsp exprsp env memory = 
  match exprsp with
  | [] -> ([],memory)
  | exprp :: rest_exprsp ->
    let (value, new_mem) = eval_exprp exprp env memory in
    let (values , final_mem) = eval_exprsp rest_exprsp env new_mem in
    (value::values, final_mem)

and eval_def def env memory =
  match def with
  | ASTConst (ident, ttype, expr) ->
      let (new_val, new_mem) = eval_expr expr env memory in
      let new_env = add_val_to_env ident new_val env in  (* Correctly update env *)
      (new_env , new_mem)

  | ASTFun (ident, ttype, arg_list, expr) ->
      let new_args = args_list_tostring arg_list in 
      let new_val = InF(expr, new_args, env) in
      let new_env = add_val_to_env ident new_val env in 
      (new_env, memory)
  
  | ASTFunRec (ident, ttype, arg_list, expr) ->
    let new_args = args_list_tostring arg_list in
    let new_val = InFR (expr, ident, new_args, env) in
    let new_env = add_val_to_env ident new_val env in
    (new_env,memory)

  | ASTVar (ident, ttype) -> 
    let (address,new_mem) = alloc memory in
    let new_env = add_val_to_env ident (InAddress address) env in
    (new_env,new_mem)

  | ASTProc(ident,argp_list, block) ->
    let new_args = argsp_list_tostring argp_list in
    let new_val = InP(block, new_args, env) in
    let new_env = add_val_to_env ident new_val env in
    (new_env,memory)

  | ASTProcRec(ident,argp_list,block) ->
    let new_args = argsp_list_tostring argp_list in
    let new_val = InPR(block, ident, new_args , env) in
    let new_env = add_val_to_env ident new_val env in 
    (new_env,memory)

and eval_stat s env memory output = 
  match s with 
  | ASTEcho e -> 
      let (new_val,new_mem) = eval_expr e env memory in 
      (new_mem, new_val :: output)
  | ASTSet(lval, expr) ->
      let (new_val, new_mem) = eval_expr expr env memory in
      let (address, new_mem2) = eval_lval lval env new_mem in
      let final_mem = add_val_to_memory new_val address new_mem2 in
      (final_mem, output)
  | ASTIfB(condition, body, alt) ->
    let (value , new_mem) = eval_expr condition env memory in
    let cond = get_bool_val value in
    if(cond)then
      let (final_mem, new_output) = eval_block body env new_mem output in
      (final_mem, new_output)
    else
      let (final_mem, new_output) = eval_block alt env new_mem output in
      (final_mem, new_output)

  | ASTWhile(condition , body) ->
    let (value,new_mem) = eval_expr condition env memory in
    let cond = get_bool_val value in
    if (not cond) then
      (new_mem, output)
    else
      let (new_mem, new_output) = eval_block body env memory output in
      let (final_mem , final_output) = eval_stat s env new_mem new_output in
      (final_mem, final_output)
  
  | ASTCall (ident, exprsp) -> 
    let (arg_values,new_mem) = eval_exprsp exprsp env memory in
    let proc = get_val_from_env ident env in
   (match proc with
    | InP(body,params,proc_env) ->
      let new_env = add_variables_to_env params arg_values proc_env in
      let (final_mem, new_output) = eval_block body new_env new_mem output in
      (final_mem, new_output)
    
    | InPR(body, name, params, proc_env) ->
      let rec_env = add_val_to_env name (InPR(body,name,params,proc_env)) proc_env in
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

  