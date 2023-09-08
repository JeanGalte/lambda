(*
Follows the exact same definition given by the BNF  
M, N ::= x | λx.M | MN

Using De Bruijin index notation, each variable is with the distance "in lambdas" of the variable with its "lambda binder".

*)
type lambda = 
	| V of int 
	| L of lambda 
	| A of (lambda * lambda)

