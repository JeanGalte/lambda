let unwrap_ex (v : 'a option) (e : exn) : 'a = 
	match v with
	| None -> raise e
	| Some x -> x
