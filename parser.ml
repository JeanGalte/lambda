
(* open Char
open List
open Identifier
open Error_g
open String_m
 *) 
exception Process_Err of string
exception Parse_Err of string

(* 
Operating on contexts
*)

(* The first member describes "the highest free variable named", or just the distance in lambdas with the beginning*)
type context = int * (char -> int)

(* Empty context *)
let nocontext = (0, fun (c : char) -> -1)

(* Know if a variable is bound or not in a context  *)
let isbound (cont : context) (c : char) : bool = (snd cont) c <> (-1)

(* Incrementing the number of free variables *)
let liftfreevar (c : context) (v : char) : context = (succ (fst c), fun (k : char) -> if k = v then fst c else (snd c) k)

(* Lifting a context when opening a new scope, when binding with a lambda *)
let liftcontext (cont : context) (c : char) : context = (succ (fst cont) , fun (k : char) -> if k = c then 0 else (if isbound cont k then (succ ((snd cont) k)) else (-1) ) )

(* Applying the context to chars*)
let applycontext (cont : context) (c : char) : int * bool = 
	let r = (snd cont) c in 
	if r = -1 
	then 
		(fst cont, false) 
	else 
		(r, true)

(* 
Operating on the lam_not_built type, which is used to build a kind of nested list in which information is stored, before becoming terms 
*)

type lam_not_built = T of lambda | N of lam_not_built list | Lam of lam_not_built

(* Adding a still-not-built term to the structure *)
let addterm (t : lambda) (l : lam_not_built) : lam_not_built = 
	match l with
	| T _ -> raise (Process_Err "Cannot add a term to a term")
	| Lam _ -> raise (Process_Err "Cannot add a term to a constructing lambda")
	| N x -> N (x @ [T t])

(* Adding a still-not-bult abstraction to the structure *)
let addabs (t : lam_not_built) (l : lam_not_built) : lam_not_built = 
	match (t, l) with
	| (N xs, N ys) -> N (ys @ [(Lam t)] ) 
	| _ -> raise (Process_Err "Left member must be abstractable, or right member must be a list")

(* Adding a still-not-built parenthesis to the structure *)
let addpar (t : lam_not_built) (l : lam_not_built) : lam_not_built = 
	match (t, l) with
	| ( N xs , N ys) -> N (xs @ [l]) 
	| _ -> raise (Process_Err "Cannot add a parenthesis if it does not output as a N ... ")

(* 
Adding an identified term. Necessary because when we write OrTrueFalse we dont mean  
\x.\y.xyx\a.\b.a\c.\d.d, but (\x.\y.xyx)(\a.\b.a)(\c.\d.d), or more simply (\x.\y.xyx)(\x.\y.x)(\x.\y.y)
*)
let addidentified (t : lambda) (l : lam_not_built) : lam_not_built = 
	match l with
	| T _ -> raise (Process_Err "Cannot add an identified term to a term") 
	| Lam _ -> raise (Process_Err "Cannot add an identified term to a lambda")
	| N x -> N (x @ [N [T t]])

(* mergining to lam_not_built *)
let merge (left : lam_not_built) (right : lam_not_built) : lam_not_built = 
	match (left, right) with
	| (N xs, N ys) -> N (xs @ ys)
	| _ -> raise (Process_Err "Cannot merge two still-not-build structures if they are not in list form")

let parse_inter (s : string) (c : context) (i : identifier) : lam_not_built = 
	let rec aux (s : char list) (c : context) (acc : lam_not_built) : lam_not_built = 	
		(
		match s with
		| x :: xs ->
			(
			match x with
			| '(' ->
				let (inpar, fin) = unwrap_ex (split_par xs) (Parse_Err "Missing ) parenthesis")  in
				let left = addpar acc (aux inpar c (N [])) in
				let right = aux fin c (N []) in
				merge left right
			| '\\' -> 
						let v = (unwrap_ex (lambda_well_written xs) (Parse_Err "The lambda term is not well written, any λ must be followed by a letter, a dot and then by a letter")) in
						if (isbound c v) 
						then 
							raise (Parse_Err ("The " ^ (Char.escaped v) ^ " is already bound")) 
						else 
							let inlamb = List.tl (List.tl xs) in
							addabs (aux inlamb (liftcontext c v) (N [])) acc
			| k when List.mem k (explode "abcdefghijklmnopqrstuvwxyz") -> 
				let (v, b) = (applycontext c k) in aux xs (if b then c else (liftfreevar c k)) (addterm (V (v)) acc)
			| k when List.mem k (explode "ABCDEFGHIJKLMNOPURSTUVWXYZ") -> 
				let (t, r) = unwrap_ex (lookforterm s i) (Parse_Err ("Unrecognized term since " ^ (join s))) in aux r c (addidentified t acc)  
			| '[' -> let t, r = unwrap_ex (lookforint xs) (Parse_Err ("Badly written int : must be written in [int] format")) in aux r c (addterm t acc) 
			| _ ->	raise (Parse_Err ("Unrecognized char : " ^ (Char.escaped x)) )
	
			)
		| [] -> acc
		)
	in aux (explode s) c (N [])


(* Applies recursively left associativity to the lam_not_built type to get the term*)
let rec apply_left_ass (l : lam_not_built) : lambda = 
	match l with
	| N [] -> raise (Parse_Err "Empty parenthesis detected")
	| T t -> t
	| Lam x -> L (apply_left_ass x)
	| N [x] -> apply_left_ass x
	| N ( (N x) :: xs ) -> apply_left_ass ( N ( (T (apply_left_ass (N x))) :: xs))
	| N ( (Lam x) :: xs) -> apply_left_ass (N ( (T (L (apply_left_ass x))) :: xs))
	| N ( (T x) :: y :: xs) -> apply_left_ass (N ((T (A ( x, apply_left_ass y))) :: xs))

let parse ?(i = default_identifier) (s : string)  : lambda = apply_left_ass (parse_inter s nocontext i) 

(* 
A faire : on fait un contexte de string dans lambda pour pouvoir faire des lambdas termes pré-enregistrés
Par convention, les termes pré-enregistrés seront écrits en majuscule (ceux contenus dans le contexte au début du parsing)
Écrire une fonction pour, dès qu'on rencontre une majuscule, aller reconnaitre le terme
*)