open Lambda
open List
open Char

(* 
To be parsed properly, identified terms must start with an uppercase letter. For example, Omega = (\x.xx)(\x.xx)
*)

type identifier = (string * lambda) list

let identify_string (s : string) (i : identifier) : lambda option = List.assoc_opt s i

let identify_term (t : lambda) (i : identifier) : string option = List.assoc_opt t (List.map (fun (x,y) -> (y,x)) i)
