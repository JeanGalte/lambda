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
-A command that led to an error 
-A print_identifier command
-A load_identifier
-A print_beta_chain command
-A let command of a var name and a term
-A remove command
-An alpha_eq command
-A beta_eq command
-A raw term 
*)

type command = E | Err of exn |I | D | P of lambda | R of string | L of (string * lambda) | A of (lambda * lambda) | B of (lambda  * lambda ) | T of lambda

let rec parse_term (i : identifier) (s : string) : lambda = 
	if String.starts_with ~prefix:"beta" s
	then 
		if String.length s = 4 then raise (Parse_Err "Missing argument to beta-reduction\n") else
		beta_red (parse_term i (takeoff_n s 4))	
	else 
	if String.starts_with ~prefix:"normalize" s
	then
		(if String.length s = 9 
		then 
			raise (Parse_Err "Missing argument for beta-normalization\n") 
		else
			(match normalize (parse_term i (takeoff_n s 9)) with
			| None -> raise No_NF
			| Some l -> l
			)
		)
	else 
		parse i s

let handle_parse_term (i : identifier) (s : string) : command = try T (parse_term i s) with e -> Err e

let parse_let (i : identifier) (s : string) : command =
	let spl = (String.split_on_char '=' (takeoff_n s 3)) in
	if List.length spl = 2 
	then
		let w, t = (String.capitalize_ascii (List.nth spl 0)), (handle_parse_term i (List.nth spl 1)) in
		match t with
		| T x -> L (w, x)
		| _ -> t
	else 
		Err (Parse_Err "Two = char seen in let expression, cannot parse\n")

let parse_betaeq (i : identifier) (s : string) : command = 
	let spl = String.split_on_char ',' (takeoff_n s 7) in
	if List.length spl = 2
	then 
		let l1, l2 = handle_parse_term i (List.nth spl 0) , handle_parse_term i (List.nth spl 0) in 
		match (l1, l2) with
		| T t1, T t2 -> B (t1, t2)
		| (Err e, _ ) -> Err e
		| (_, Err e) -> Err e
		| _ -> Err (Parse_Err "Something went wrong with alpha equivalence testing")
	else 
		Err (Parse_Err "Bad syntax for beta equivalence. Use it this way : beta_eq [term 1] , [term 2] \n")


let parse_rm (s : string) : command = R (takeoff_n s 2)

let parse_beta_chain (i : identifier) (s : string) : command = 
	let t = handle_parse_term i (takeoff_n s 16)  in
	match t with
	| T x -> P x
	| _ -> t
	
let parse_alphaeq (i : identifier) (s : string) : command = 
	let spl = (String.split_on_char ',' (takeoff_n s 8)) in 
	if List.length spl = 2 
	then
		let l1, l2 = handle_parse_term i (List.nth spl 0) , handle_parse_term i (List.nth spl 0) in 
		match (l1, l2) with
		| T t1, T t2 -> A (t1, t2)
		| (Err e, _ ) -> Err e
		| (_, Err e) -> Err e
		| _ -> Err (Parse_Err "Something went wrong with alpha equivalence testing")
	else 
		Err (Parse_Err "Bad syntax for alpha equivalence. Use it this way : alpha_eq [term 1] , [term 2] \n")

let parse_command (i : identifier) (s : string) : command  =
	let ts = trimspaces s in 
	match ts with
	| _ when String.starts_with ~prefix:"exit" ts ->  E
	| _ when String.starts_with ~prefix:"identifier" ts -> I
	| _ when String.starts_with ~prefix:"print_beta_chain" ts -> parse_beta_chain i ts
	| _ when String.starts_with ~prefix:"rm" ts -> parse_rm ts
	| _ when String.starts_with ~prefix:"let" ts ->  parse_let i ts 
	| _ when String.starts_with ~prefix:"load_default_id" ts -> D
	| _ when String.starts_with ~prefix:"alpha_eq" ts -> parse_alphaeq i ts
	| _ when String.starts_with ~prefix:"beta_eq" ts -> parse_betaeq i ts
	| _ -> handle_parse_term i ts

let rec main (i : identifier) : unit = 
	print_string "λT>" ; 
	let c = parse_command i (read_line ()) in
	match c with
	| E -> print_string "λT exited" ; exit 0
	| I -> print_identifier i ; main i
	| D -> print_identifier default_identifier; main default_identifier
	| P l -> print_beta_chain i l; main i
	| R s -> main (remove_term s i)
	| L (s, l) -> print_lambda l [] ; print_newline () ; main (try (add_term s l i) with Identifier_err err -> print_string err; print_newline (); i) 
	| T l -> print_lambda l [] ; print_newline (); main i
	| A (l1, l2) -> (if l1 = l2 then print_string "True" else print_string "False") ; print_newline () ; main i
	| B (l1, l2) -> (if beta_eq l1 l2 then print_string "True" else print_string "False") ; print_newline () ; main i
	| Err e ->
		(
		match e with
		| No_NF -> print_string "No normal form for this term\n"
		| Parse_Err x -> print_string ("Parsing Error : " ^ x ^ "\n")
		| Max_Beta_Red -> print_string "Maximum beta reduction depth reached\n"
		| Term_Parse_Err x -> print_string ("Term Parsing Error : " ^x^"\n") 
		| _ -> print_string "Another unexepected error happened : \n" ; raise e
		) ; main i

let () = print_endline "" ; main [] ;;
