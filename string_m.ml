open List
(* 
Operating on strings, or char list
*)

(*
splits an expression with the corresponding parenthesis ) 
the None case corresponds to a missing parenthesis
*)
let split_par (l : char list) : (char list * char list) option  = 
	let rec aux (l : char list) (acc : char list) (n : int) : (char list * char list) option =
		match l with
		| x :: xs when x = ')'  -> if n = 0 then Some (List.rev acc, xs) else aux xs (x :: acc) (pred n)
		| x :: xs when x = '(' -> aux xs (x :: acc) (succ n)		
		| x :: xs -> aux xs (x :: acc) n
		| [] -> None 
	in aux l [] 0

(* Exploding the original string to a char list *)
let explode (s : string) : char list =
  let rec exp (i : int) (l : char list)  : char list =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(* When \ is written, we check that it is followed by a var name and a point, and something else which is a letter *)
let lambda_well_written (s : char list) : char option = 
	match s with
	| x :: y :: z :: xs when (List.mem x (explode "abcdefghijklmnopqrstuvwxyz")) && y = '.' && (List.mem z (explode "abcdefghijklmnopqrstuvwxyz(\\ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890")) -> Some	x
	| _ -> None

(* Joining a char list *)
let join (s : char list) : string = List.fold_right (^) (List.map Char.escaped s) ""