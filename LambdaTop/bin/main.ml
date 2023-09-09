open LambdaTop
open Identifier
open Parser
open Lambda
open External
open String_m
open Beta

exception Parse_Err of string

(* exception Parse_Err of string *)

(*
A command can be :
-An exit command
-A raw term (like when you juste write 1 in utop, or beta_red Succ[1])
-A let command 
*)

let rec parse_term (i : identifier) (s : string) : lambda = 
	if String.starts_with ~prefix:"beta" s
	then 
		beta_red (parse_term i (takeoff_n s 4))	
	else 
		parse i s

let parselet (i : identifier) (s : string) : identifier =
	let spl = String.split_on_char '=' s in
	if List.length spl = 2 
	then
		let w, t = (List.nth spl 0), (List.nth spl 1) in
		let p = (parse_term i t) in 
		print_string (w ^ " : ") ; print_lambda p [] ; print_endline "";
		try (add_term (String.capitalize_ascii w) (parse_term i t) i) with Identifier_err s -> (print_string s ; print_endline ""; i)
	else 
		raise (Parse_Err "Two = char seen in let expression")

let handle_let (i: identifier) (s : string) : identifier =
	if String.starts_with ~prefix:"let" s
	then 
		parselet i (takeoff_n s 3)
	else 
		i

let handle_exit (_ : identifier) (s : string) : unit = if String.equal s "exit" then exit 0 else ()

let handle_print_identifier (i : identifier) (s : string) : unit =
	if String.starts_with ~prefix:"identifier" s
	then 
		print_identifier i
	else 
		()

let handle_beta (_ : identifier) (_ : string) : unit = ()

let parse_command (i : identifier) (s : string) : identifier =
	let ts = trimspaces s in 
	let () = handle_exit i ts in
	let () = handle_print_identifier i ts in
	let () = handle_beta i ts in
	let ni = handle_let i ts in
	ni

let rec main (i : identifier) : unit = 
	print_string " λT >" ; 
	let ni = parse_command i (read_line ()) in
	main ni

let () = print_endline "" ; main [] ;;
