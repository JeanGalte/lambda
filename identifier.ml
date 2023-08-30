(* open Lambda *)
open List

type identifier = (string * lambda) list

let identify_string (s : string) (i : identifier) : lambda option = List.assoc_opt s i

let identify_term (t : lambda) (i : identifier) : string option = List.assoc_opt t (List.map (fun (x,y) -> (y,x)) i)

