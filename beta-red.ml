(* open Lambda *)
(* open Substitution *)

(* Evaluates to true if and only if the term is in normal form for the beta-reduction*)
let is_normal (l : lambda) : bool = 
	match l with
	| V _ | L _ -> true
	| _ -> false 

(*
Reduces the term, taking out a single β-redex for each call of the beta_red function.

Looks for rules in this order : 
1) β
2) cong1
3) cong2
4) ξ

It is important to note that this is not strictly a beta-reduction function, because a term in normal-form does not beta-reduce to itself while for example, bet_red (V 1) evaluates to V 1.
*)
let rec beta_red (l : lambda ) : lambda = 
	match l with
	| A (L t,l2) -> print_string "1 \n" ; subs t (upenv ind l2)
	| A (t, v) when is_normal t -> print_string "2 \n"; A (t, beta_red v)
	| A (t, v) ->  print_string "3 \n"; A (beta_red t, v)
	| L t  when not (is_normal t) -> print_string "4 \n"; L (beta_red t)
	| _ -> print_string "end with\n"; l 

(* Evaluates to the beta-reduct of a term, evaluates to None since a term is stable by beta-reduction*)
let rec normalize (l : lambda) : lambda option =
	if is_normal l 
	then 
		Some l 
	else 
		let red = beta_red l in 
			if (red = l) 
			then
				None
			else 
				normalize red

