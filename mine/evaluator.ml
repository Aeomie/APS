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

let prim1 arg = not_op_func arg;;
let prim2 ident arg1 arg2 env = 
  match ident with
  | `eq -> eq_func [arg1;arg2]
  | `lt -> lt_func [arg1;arg2]
  | `add -> add_func [arg1;arg2]
  | `sub -> sub_func [arg1;arg2]
  | `mul -> mul_func [arg1;arg2]
  | `div -> div_func [arg1;arg2]
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
let rec eval_expr e env = 
  match e with
  | ASTId("true")-> InZ(1)
  | ASTId("false") -> InZ(0)
  | ASTNum n -> InZ n
  | ASTId s -> get_val s env
  | _ ->  failwith " Unknown expression"
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

let eval_cmd cmd env output = 
  match cmd with
  | ASTStat stat -> eval_stat stat env output
  | ASTDef (_, _) -> output
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
      print_string "Evaluator.\n"
  with Lexer.Eof ->
    exit 0
        