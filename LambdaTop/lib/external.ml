open Lambda
open Identifier

(* Directly taken and modified from https://ocaml.org/docs/formatting-text *)
let rec print_exp0 (l : lambda) (i : identifier) : unit = 
	let r = identify_term l i in 
	if r = None 
	then 
		(match l with
		| V i -> print_int i
		| t -> Format.open_hovbox 1; print_string "(" ; print_lambda t i; print_string ")" ;  Format.close_box ()
		)
	else 
		print_string (Option.get r)
and print_other_applications (l : lambda) (i : identifier) : unit = 
	match l with
	| A (f, arg) -> print_app f i; print_exp0 arg i
	| f -> print_exp0 f i
and print_app (l : lambda) (i : identifier) : unit = 
	let r = identify_term l i in 
	if r = None 
	then 
		(match l with
		| a -> Format.open_hovbox 2; print_other_applications a i; Format.close_box ()
		)
	else 
		print_string (Option.get r)
and print_lambda (l : lambda) (i : identifier) : unit = 
	let r = identify_term l i in 
	if r = None 
	then 
		(match l with
		| L t -> Format.open_hovbox 1; print_string "λ"; print_lambda t i
		| a -> print_app a i
		)
	else 
		print_string (Option.get r) 

let rec print_id (i : identifier) : unit = 
	match i with
	| [] -> ()
	| (s, t) :: xs -> 
		let () = print_string s in
		let () = print_string " = " in
		let () = print_lambda t [] in
		let () = print_newline () in
		print_id xs

let print_identifier (i : identifier) : unit = print_string "________________________________________\n" ; print_id i; print_string "________________________________________\n"