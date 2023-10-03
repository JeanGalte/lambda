(* 
Operating on strings, or char list
*)

let reserved_word = ["load";"let";"identifer"]

let word_valid (s : string) : bool = List.mem s reserved_word

(*
splits an expression with the corresponding parenthesis ) 
the None case corresponds to a missing parenthesis
*)

(* Thanks chatgpt. I have no clue why there is no such function in stdlib tbh *)
let trimspaces (s : string) : string =
  let rec aux acc = function
    | [] -> String.concat "" (List.rev acc)
    | ' ' :: rest -> aux acc rest
    | c :: rest -> aux (String.make 1 c :: acc) rest
  in
  aux [] (String.to_seq s |> List.of_seq)

let takeoff_n (s : string) (n : int) : string =
  let l = String.length s in
  if n >= l then
    ""
  else
    String.sub s n (l - n)


let split_par (l : char list) : (char list * char list) option  = 
	let rec aux (l : char list) (acc : char list) (n : int) : (char list * char list) option =
		match l with
		| x :: xs when x = ')'  -> if n = 0 then Some (List.rev acc, xs) else aux xs (x :: acc) (pred n)
		| x :: xs when x = '(' -> aux xs (x :: acc) (succ n)		
		| x :: xs -> aux xs (x :: acc) n
		| [] -> None 
	in aux l [] 0

(*
Exploding the original string to a char list 
Should be used on raw string, will bug meeting \n, \r, \t, \b otherwise.
*)
let explode (s : string) : char list =
  let rec exp (i : int) (l : char list)  : char list =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(* When \ is written, we check that it is followed by a var name and a point, and something else which is a letter *)
let lambda_well_written (s : char list) : char option = 
	match s with
	| 'a'..'z' as x :: '.' :: ('a'..'z' | 'A'..'Z' | '0'..'9' | '\\' | '(' | '[') :: _-> Some x
	| _ -> None

(* Joining a char list *)
let join (s : char list) : string = List.fold_right (^) (List.map Char.escaped s) ""