(* #open Lambda *)

(* The mechanism needed for substitution with de Bruijin index notation : when we do a substitution, we juste "lift" the environment *)

let ind = (fun i -> V i) 

(* Lifting up the environment *)
let upenv (env : int -> lambda) (t : lambda) (i : int) : lambda =  
	match i with
	| 0 -> t
	| _ -> env (i-1)

(* Usual subsitution, described in Selinger *)
let rec subs (l : lambda) (env : int -> lambda) : lambda = 
	match l with
	| V i -> env i
	| L t -> L (subs t (fun (i : int) -> if i = 0 then (V 0) else subs (env (i-1)) (fun (j : int) -> V (succ j)))) 
	| A (l1, l2) -> A ((subs l1 env), (subs l2 env))
