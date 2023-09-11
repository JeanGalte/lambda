open Lambda

let ind (l : lambda) : int -> lambda =  (fun v -> if v = 0 then l else V (pred v))

let rec lift_i (i : int) (n : int) (l : lambda) : lambda = 
	match l with
	| V k -> if k < i then l else V (k + n)
	| L t -> L (lift_i (succ i) n t)
	| A (l1, l2) -> A ( lift_i i n l1, lift_i i n l2) 

let lift (n : int) (l : lambda) = lift_i 0 n l

let rec subst_i (i : int) (env : int -> lambda) (l : lambda) : lambda = 
	match l with
	| V k -> if k < i then l else lift i (env (k-i))
	| L t -> L (subst_i (succ i) env t)
	| A (l1, l2) -> A (subst_i i env l1, subst_i i env l2)

let subs  (env : int -> lambda) (l : lambda) : lambda = subst_i 0 env l