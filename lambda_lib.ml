open Lambda

(* A lib with usual lambda terms *)
let identifier (s : string) : lambda = 
	match s with
	| k when k = "Omega" ->  A (L (A (V 1, V 1)), L (A (V 1, V 1)))
	| k when k = "If_then_else" -> L (V 1)
	| k when k = "Y" -> L (A (L (A (V 2, A (V 1, V 1))), L (A (V 2, A (V 1, V 1)))))
