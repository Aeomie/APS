module StringMap = Map.Make(String)

type environment = string StringMap.t  (* A map from strings to strings *)

let env : environment = StringMap.empty  (* Create an empty environment *)

(* Add some values to the environment *)
let env = StringMap.add "key1" "lul" env
let env = StringMap.add "key2" "lul2" env
let env = StringMap.add "key3" "lul3" env

(* Retrieve a value by key *)
let value = StringMap.find_opt "key2" env
let () = match value with
  | Some v -> Printf.printf "Found: %s\n" v  (* Corrected: Print the string value *)
  | None -> Printf.printf "Key not found\n"

(* Iterate through the environment *)
let () = 
  StringMap.iter (fun key value -> Printf.printf "%s -> %s\n" key value) env  (* Corrected: Use %s for both key and value *)
