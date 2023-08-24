open Lambda

(* not fully working, adding too much parenthesis *)
let rec to_string (l : lambda) : string = 
	match l with
	| V i -> string_of_int i
	| L x -> "λ" ^ (to_string x)
	| A (l1, l2) -> "(" ^  (to_string l1) ^ ")" ^ "(" ^ (to_string l2) ^ ")"