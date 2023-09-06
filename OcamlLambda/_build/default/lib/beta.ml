open Lambda
open Substitution

(* Evaluates to true if and only if the term is in normal form for the beta-reduction*)
let rec is_beta_normal (l : lambda) : bool = 
	match l with
	| V _ -> true
	| L x -> is_beta_normal x
	| A (L _, _) -> false
	| A (l1, l2) -> is_beta_normal l1 && is_beta_normal l2
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
	| A (L t,l2) -> subs t (upenv ind l2)
	| A (t, v) when is_beta_normal t -> A (t, beta_red v)
	| A (t, v) -> A (beta_red t, v)
	| L t  when not (is_beta_normal t) -> L (beta_red t)
	| _ -> l 

(* Get the beta-reduction "chain" for a certain length*)
let get_beta_red_chain ?(lim = 100) (l : lambda) : lambda list = 
	let rec aux (l : lambda) (acc : lambda list) (lim  :int) : lambda list = 
		match l with
		| A (L t,l2) -> aux (subs t (upenv ind l2)) (acc @ [l]) (pred lim)
		| A (t, v) when is_beta_normal t -> aux (A (t, beta_red v)) (acc @ [l]) (pred lim)
		| A (t, v) -> aux (A (beta_red t, v)) (acc @ [l]) (pred lim)
		| L t  when not (is_beta_normal t) -> aux (L (beta_red t)) (acc @ [l]) (pred lim)
		| _ -> acc
	in aux l [] lim 
(* 
Evaluates to the beta-reduct of a term, evaluates to None since a term is stable by beta-reduction.
Does not terminate always ! Evaluating in None guarantees that your term has no normal form, but the opposite is not true : your term can have no normal form and not evaluate to None with this algorithm. 
For example, a n-cylce is not evaluating to None if n >2.  
*)
let rec normalize (l : lambda) : lambda option =
	if is_beta_normal l 
	then 
		Some l 
	else 
		let red = beta_red l in 
			if (red = l) 
			then
				None
			else 
				normalize red

(* 
Again, is not guaranteed to terminate.
For termes with no normal form, it does not give beta equivalence : it only looks for a term that they can both reach with 100 beta reductions (it is a default parameter) 
*)
let beta_eq (l1 : lambda) (l2 : lambda) : bool = 
	match (normalize l1, normalize l2) with
	| (None, None) -> 
	let module Lamb = Set.Make(struct type t = lambda let compare (l1 : t) (l2 : t) = compare l1 l2 end) in
		let s1 = Lamb.of_list (get_beta_red_chain l1) in
		let s2 = Lamb.of_list (get_beta_red_chain l2) in
		(Lamb.inter s1 s2 = Lamb.empty)  
	| (Some x, Some y) -> (x = y)
	| _ -> false  (* By a corollary of Church-Rosser theorem, if M =β N, then neither or both have a β-normal form*)