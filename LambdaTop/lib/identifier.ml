open Lambda
open String_m

(* 
To be parsed properly, identified terms must start with an uppercase letter. For example, Omega = (\x.xx)(\x.xx)
*)
exception Identifier_err of string

exception Nan_church

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

let remove_term (s : string) (i : identifier) : identifier = 
	let s_i = (List.map fst i) in
	let elem = List.find_opt (String.equal s) s_i in
	if elem = None 
	then
		raise (Identifier_err ("Cannot remove " ^s^ " from the identifier : no such term is contained"))
	else 
		List.filter (fun (a,_) -> a <> s) i

let default_identifier = 
	[
	("If_then_else", L (V 0));
	("Omega",A (L (A (V 0, V 0)), L (A (V 0, V 0))));	
	("Yc",L (A (L (A (V 1, A (V 0, V 0))), L (A (V 1, A (V 0, V 0))))));
	("Theta",A (L (L (A (V 0, A (A (V 1, V 1), V 0)))),L (L (A (V 0, A (A (V 1, V 1), V 0))))));
	("Sc",L (L (L (A (A (V 2, V 0), A (V 1, V 0))))));
	("False",L (L (V 0)));
	("True",L (L (V 1)));
	("Or",L (L (A (A (V 1, V 0), V 1))));
	("And", L (L (A (A (V 1, V 0), V 1))));
	("Not",  L (A (A (V 0, L (L (V 1))), L (L (V 0)))));
	("Succ", L (L (L (A (V 1, A (A (V 2, V 1), V 0))))));
	("Add",L (L (L (A (V 2, A (V 1, V 0))))));
	("Iszero",L (L (L (A (A (V 2, L (V 1)), V 1)))));
	("Pred", L (L (L (A (A (A (V 2, L (L (A (V 0, A (V 1, V 3))))), L (V 1)), L (V 0))))));
	("Mult",  L (L (L (A (V 2, A (V 1, V 0))))));
	] 

let rec n_church (n : int) : lambda = 
	if n = 0 
	then
		L (L (V 0))
	else 
		let p = n_church (n-1) in
		match p with
		| L (L x) -> L (L (A ( V 1, x)))
		| _ -> raise Nan_church

let lookforint (c : char list) : (lambda * char list) option =
	let rec aux (c : char list) (acc : string) : (lambda * char list) option = 
		match c with
		| x :: xs when List.mem x (explode "1234567890") -> aux xs (acc ^ Char.escaped x)
		| x :: xs when x = ']' -> Some ( n_church (int_of_string acc), xs) 
		| [] -> None
		| _ -> None
	in aux c ""

