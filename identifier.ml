(* 
open Lambda
open Char
open String_m
 *)
(* 
To be parsed properly, identified terms must start with an uppercase letter. For example, Omega = (\x.xx)(\x.xx)
*)

exception Identifier_err of string

type identifier = (string * lambda) list

let identify_string (s : string) (i : identifier) : lambda option = List.assoc_opt s i

let identify_term (t : lambda) (i : identifier) : string option = List.assoc_opt t (List.map (fun (x,y) -> (y,x)) i)

(*
You don't want to have terms with a name contained in another term's name.
for example, you don't want to have a term named Omega and another one named Omega_bis in your context, otherwise when parsing Omega_bis, you will meet a bug
*)
let lookforterm (l : char list) (i : identifier) : (lambda * char list) option = 
	let rec aux (l : char list) (acc : char list) : (lambda * char list) option = 
		match l with
		| [] -> None
		| x :: xs ->
			let m = acc @ [x] in
			let m_i = identify_string (join m) i in 
			if m_i <> None 
			then
				Some (Option.get m_i, xs)
			else 
				aux xs m  
	in aux l [] 

let add_term (s : string) (t : lambda) (i : identifier) : identifier = 
	let s_i = (List.map fst i) in 
	if List.filter (String.starts_with ~prefix:s) s_i <> [] || List.exists (fun k -> String.starts_with ~prefix:k s) s_i 
	then 
		raise (Identifier_err ("Cannot add " ^ s ^" to the identifier. Another registered term starts with " ^ s ^ " or " ^ s ^ " starts with another term from the identifier, which might cause bugs."))
	else
		((s,t) :: i)


let default_identifier = 
	[
	("If_then_else", L (V 1));
	("Omega",A (L (A (V 1, V 1)), L (A (V 1, V 1))));	
	("Yc",L (A (L (A (V 2, A (V 1, V 1))), L (A (V 2, A (V 1, V 1))))));
	("Theta",A (L (L (A (V 1, A (A (V 2, V 2), V 1)))),L (L (A (V 1, A (A (V 2, V 2), V 1))))));
	("Sc",L (L (L (A (A (V 3, V 1), A (V 2, V 1))))));
	("False",L (L (V 2)));
	("True",L (L (V 1)));
	("Or",L (L (A (A (V 2, V 2), V 1))));
	("And", L (L (A (A (V 2, V 1), V 2))));
	("Not",  L (A (A (V 1, L (L (V 2))), L (L (V 1)))));
	("Succ", L (L (L (A (V 2, A (A (V 3, V 2), V 1))))));
	("Add",L (L (L (L (A (V 3, A (A (V 4, V 2), V 1)))))));
	("Iszero",L (L (L (A (A (V 3, L (V 2)), V 2)))));
	("Pred", L (L (L (A (A (A (V 3, L (L (A (V 1, A (V 2, V 4))))), L (V 2)), L (V 1))))));
	("Mult",  L (L (L (A (V 3, A (V 2, V 1))))));
	] 
