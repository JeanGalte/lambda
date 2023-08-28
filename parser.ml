open Char
open List
(* 
Liste des tokens :
- λ (binder) , bind la variable qui vient immédiatement après. Je vais essayer de définir son comportement précisément 
- . (séparateur), sépare la variable bind par un  λ et le terme associé au bind
- ( et ) (séparateurs) séparent deux termes 
- des littéraux qui correspondent à des termes déjà faits genre top, bottom, 1, 2 , S, K, Theta etc 
- le reste (les variables genre x, y, z) 
 *)

exception Process_Err of string
exception Parse_Err of string

type lambda = 
	| V of int 
	| L of lambda 
	| A of (lambda * lambda)

type context = (int * char -> int option)

(* Lifting a context when opening a new scope, when binding with a lambda *)
let liftcontext (cont : context) (c : char) : context = fun (k : char) -> if k = c then 1 else succ (cont k)

let applycontext (cont : context) (c : char) : int = 
	match cont c with
	| None -> fst cont
	| Some x -> x

let nocontext = fun (c : char) -> None 

let isbound (cont : context) (c : char) = (cont c != None)

type lam_not_built = T of lambda | N of lam_not_built list | L of lam_not_built

(* Adding a still-not-built term to the structure *)
let addterm (t : lambda) (l : lam_not_built) : lam_not_built = 
	match l with
	| T _ -> raise (Process_Err "Cannot add a term to a term")
	| L _ -> raise (Process_Err "Cannot add a term to a constructing lambda")
	| N x -> N (x @ [T t])

(* Exploding the original string to a char list *)
let explode (s : string) : char list =
  let rec exp (i : int) (l : char list)  : char list =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(* When \ is written, we check that it is followed by a var name and a point, and something else which is a letter *)
let lambda_well_written (s : char list) : (char * bool) = 
	match s with
	| x :: y :: z :: xs -> 
		(x, (List.mem x (explode "abcdefghijklmnopqrstuvwxyz")) && y = '.' && (List.mem z (explode "abcdefghijklmnopqrstuvwxyz\\")))  
	| _ -> raise (Parse_Err "The lambda term is not well written, any λ must be followed by a letter, a . and then by a letter. Here a λ is followed by less than 2 chars.")



let parse_inter (s : string) : lam_not_built = 
	let rec aux (s : char list) (c : context) (acc : lam_not_built) : lam_not_built = 	
		(
		match s with
		| x :: xs ->
			(
			match x with
			| '\\' -> 
						let (v, w) = lambda_well_written xs in
						if not w then Parse_Err
						
			| k when List.mem k (explode "abcdefghijklmnopqrstuvwxyz") -> aux xs c (addterm (V (c x)) acc)
			| _ ->	raise (Parse_Err ("Unrecognized char : " ^ (Char.escaped x)) )
			)
		| [] -> acc
		)
	in aux (explode s)  (N [])