module EnvMap = Map.Make(String)

type value = 
  | inZ of int
  | inF of expr * string list (* here should be p , but idk whats p yet*) (* string list because of the variables*)
  | inFR of expr * string list * string (* here p too*) 

type env = value EnvMap.t  (* Maps a string (variable name) to a value *)


let empty_env : env = EnvMap.empty

(* following this principal , tommorow add not lt mul div eq just like shown in the cours*)

exception MyError of string;;  (* Defines an exception that takes a string argument *)

