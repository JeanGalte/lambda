open Lambda

(* Printer, not fully working,  printing too much parenthesis *)
let rec print_lamb_brujin (l : lambda) : unit = 
	match l with
	| V i -> print_int i
	| L x -> print_string "(" ;  print_string "λ" ; print_lamb_brujin x ; print_string ")" 
	| A (l1 , l2) -> (print_string "("; print_lamb_brujin l1 ; print_lamb_brujin l2; print_string ")")
