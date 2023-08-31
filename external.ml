open Lambda

(* not fully working, adding too much parenthesis *)
let rec to_string (l : lambda) : string = 
	match l with
	| V i -> string_of_int i
	| L x -> "λ" ^ (to_string x)
	| A (l1, l2) -> "(" ^  (to_string l1) ^ ")" ^ "(" ^ (to_string l2) ^ ")"

let print_lamb (l : lambda) : unit = print_string (to_string l)

(*
With identifier : meaning the term  A (L (A (V 1, V 1)), L (A (V 1, V 1))) will be evaluated as "Omega". 
*)

let rec to_string_identifier (l : lambda) (i : identifier) : string = 
	match (identify_term l i, l) with
	| (Some x, _) -> x
	| (None, V x) -> string_of_int x
	| (None, L x) ->  "λ" ^ (to_string_identifier x i)
	| (None, A (l1 ,l2)) -> "(" ^  (to_string_identifier l1 i) ^ ")" ^ "(" ^ (to_string_identifier l2 i) ^ ")"

let print_lamb_identifier (l : lambda) (i : identifier) : unti = print_string (to_string_identifier l i)