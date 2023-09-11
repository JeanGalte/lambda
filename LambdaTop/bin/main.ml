open LambdaTop
open Identifier
open Parser
open Lambda
open External
open String_m
open Beta

exception Parse_Err of string

(*
A command can be :
-An exit command
-A print_identifier command
-A load_identifier
-A print_beta_chain command
-A let command of a var name and a term
-A remove command
-A raw term 

*)

type command = E | I | D | P of lambda | R of string | L of (string * lambda) | T of lambda

let rec parse_term (i : identifier) (s : string) : lambda = 
	if String.starts_with ~prefix:"beta" s
	then 
		if String.length s = 4 then raise (Parse_Err "Missing argument to beta\n") else
		beta_red (parse_term i (takeoff_n s 4))	
	else 
		parse i s

let parse_let (i : identifier) (s : string) : command =
	let spl = (String.split_on_char '=' (takeoff_n s 3)) in
	if List.length spl = 2 
	then
		let w, t = (String.capitalize_ascii (List.nth spl 0)), (parse_term i (List.nth spl 1)) in
		L (w, t)
	else 
		raise (Parse_Err "Two = char seen in let expression, cannot parse")

let parse_rm (s : string) : command = R (takeoff_n s 2)

let parse_beta_chain (i : identifier) (s : string) : command = 
	P (parse i (takeoff_n s 16))

let parse_command (i : identifier) (s : string) : command option =
	let ts = trimspaces s in 
	match ts with
	| _ when String.starts_with ~prefix:"exit" ts -> Some E
	| _ when String.starts_with ~prefix:"identifier" ts -> Some I
	| _ when String.starts_with ~prefix:"print_beta_chain" ts -> Some (parse_beta_chain i ts)
	| _ when String.starts_with ~prefix:"rm" ts -> Some (parse_rm ts)
	| _ when String.starts_with ~prefix:"let" ts -> Some (parse_let i ts) 
	| _ when String.starts_with ~prefix:"load_default_id" ts -> Some D
	| _ -> (try Some (T (parse_term i ts)) with Parse_Err err | Term_Parse_Err err-> (print_string err ; print_newline (); None))

let rec main (i : identifier) : unit = 
	print_string "λT>" ; 
	let c = parse_command i (read_line ()) in
	match c with
	| Some c -> 
	(
		match c with
		| E -> print_string "λT exited" ; exit 0
		| I -> print_identifier i ; main i
		| D -> print_identifier default_identifier; main default_identifier
		| P l -> print_beta_chain i l; main i
		| R s -> main (remove_term s i)
		| L (s, l) -> print_lambda l [] ; print_newline () ; main (try (add_term s l i) with Identifier_err err -> print_string err; print_newline (); i) 
		| T l -> print_lambda l [] ; print_newline (); main i
	)	
	| None -> main i

let () = print_endline "" ; main [] ;;
