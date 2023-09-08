open OcamlLambda
open Identifier
(* open Parser
open Lambda
 *)open External
open String_m

(* exception Parse_Err of string *)

(*
A command can be :
-An exit command
-A raw term (like when you juste write 1 in utop, or beta_red Succ[1])
-A let command 
*)

(* let parselet (i : identifier) (s : string) : identifier =
	let spl = String.split_on_char '=' s in
	if List.length spl = 2 
	then
		let w, t = (List.nth spl 1), (List.nth spl 2)
		add_term (String.capitalize_ascii w) (parse i t) i
	else 
		raise (Parse_Err "Two = char seen in let expression")

let handle_let (i: identifier) (s : string) : identifier =
	if String.starts_with ~prefix:"let"
	then 
		parselet i s
	else 
		i *)

let handle_exit (_ : identifier) (s : string) : unit = if String.equal s "exit" then exit 0 else ()

let rec print_identifier (i : identifier) : unit = 
	match i with
	| [] -> ()
	| (s, t) :: xs -> print_string s ; print_string " = " ; print_lambda t [] ; (print_endline "") ; print_identifier xs

let handle_print_identifier (s : string) (i : identifier) : unit =
	if String.starts_with ~prefix:"identifier" s
	then 
		print_identifier i
	else 
		()

let parse_command (i : identifier) (s : string) : identifier =
	let ts = trimspaces s in 
	let () = handle_exit i ts in
	let () = handle_print_identifier ts i in
	i

let rec main (i : identifier) : unit = 
	print_string " λT >" ; 
	let ni = parse_command i (read_line ()) in
	print_endline ""; 
	main ni

let () = main [] ;;
