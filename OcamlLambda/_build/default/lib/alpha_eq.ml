open Parser

let alpha_eq (l1 : string) (l2 : string) : bool = ((parse l1) = (parse l2))