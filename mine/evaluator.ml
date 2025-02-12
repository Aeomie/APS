
type value = 
  | inZ of int
  | inF of expr * string list (* here should be p , but idk whats p yet*) (* string list because of the variables*)
  | inFR of expr * string list * string (* here p too*) 


(* following this principal , tommorow add not lt mul div eq just like shown in the cours*)