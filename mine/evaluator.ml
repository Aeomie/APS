open Ast

module StringMap = Map.Make(String)

type value = 
  | InZ of int
  | InF of expr * string list * env(* here should be p , but idk whats p yet*) (* string list because of the variables*)
  | InFR of expr * string * string list * env (* here p too*) 

(* following this principal , tommorow add not lt mul div eq just like shown in the cours*)

type environment = value StringMap.t

let env : environment = StringMap.empty



(* type aps_val = 
|inPrim of (aps_val list -> aps_val) *)

(* check if the expr exist*)
  (* might remove this *)
let rec check_expr e =
  match (StringMap.find_opt e env) with
  |Some _ -> true
  |None -> false

(* to get the value from the base environment*)
let get_val ident =
  if String.length ident > 0 then
    try
      StringMap.find ident env
    with
    | _ -> failwith ("No value for string: " ^ ident)
  else
    failwith "Identifier is empty"


(* expression evaluator*)
let rec eval_expr e = 
  match e with
  | ASTNum n -> inZ n
  | AST s -> get_val s
  | ASTand (e1,e2) -> wrap and_ e1 e2
  | ASTOr (e1,e2) -> wrap or_ e1 e2
  | ASTif (e1,e2,e3) -> wrap if_ e1 e2 e3
  | ASTLambda (args,e) -> 
  | _ ->  failwith " Unknown expression"




(* init functions *)
let not_op arg =
  match arg with
  | InZ(0) -> InZ(1)
  | InZ(_) -> InZ(0)
  | _ -> assert false

let eq args = 
  match args with
  | [InZ(a); InZ(b)] -> if a = b then InZ(1) else InZ(0)
  | _ -> assert false 

let lt args = 
  match args with
  | [InZ(a); InZ(b)] -> if a < b then InZ(1) else InZ(0)
  | _ -> assert false 

let add args = 
  match args with
  | [InZ(a); InZ(b)] -> InZ(a + b)
  | _ -> assert false 

let sub args =
  match args with
  | [InZ(a); InZ(b)] -> InZ(a - b)
  | _ -> assert false

let mul args =
  match args with
  | [InZ(a); InZ(b)] -> InZ(a * b)
  | _ -> assert false
    
let div args =
  match args with
  | [InZ(a); InZ(b)] when b <> 0 -> InZ(a / b)
  | _ -> assert false 

(* ajout des fonctions à l'environnement *)

(* passer de num à int*)
let rec num_of_int n = 
  int_of_string n



(* functions for evaluations *)
let and_func args =
  match args with
  | [InZ(a); InZ(b)] ->
    if (check_expr a) = 0 then 
      Inz(0)
    else
      Inz(check_expr b) 
  | _ -> failwith "Invalid arguments for and operation"

let or_func args =
  match args with
  | [InZ(a); InZ(b)] ->
    if (check_expr a) = 1 then 
      Inz(1)
    else
      Inz(check_expr b) 
  | _ -> failwith "Invalid arguments for and operation"

let if_func args =
  match args with
  | [InZ(a); InZ(b); Inz(c)] ->
    if (check_expr a) = 1 then 
      Inz(check_expr b) 
    else
      Inz(check_expr c) 
  | _ -> failwith "Invalid arguments for and operation"


let rec add_args args =
  match args with

let app f args =
  |


  (*l'evaluateur renvoie une liste d'entier de ce qui est affiché avec echo *)


(* function wrappers *)

(* wrapper for 2 parameters*)
let wrap ident arg1 arg2 = 
  match ident with
  | eq_ -> eq [arg1:arg2]
  | lt_ -> lt [arg1:arg2]
  | add_ -> add [arg1:arg2]
  | sub_ -> sub [arg1:arg2]
  | mul_ -> mul [arg1:arg2]
  | div_ -> div [arg1:arg2]
  | and_ -> and_func [arg1:arg2]
  | or_ -> or_func [arg1:arg2]
  | _ -> failwith "Invalid function name"


(* wrapper for 3 parameters *)
let wrap2 ident arg1 arg2 arg3 =
  match ident with
  | if_ -> if_func [arg1:arg2:arg3]
  | _ -> failwith "no function with that name and 3 parameters"
let rec eval_ttype tt =
  match tt with
  |ASTBool b -> 
    if b = true then inZ(1)
    else inZ(0)
  |ASTInt i-> intZ(num_of_int n)
  |ASTArrow list ->
    match list with
    |[] -> 
    |t::ll ->



