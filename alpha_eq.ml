open Lambda
open Parser

let alpha_eq (l1 : string) (l2 : string) : bool = (parse_nature l1) = (parse_nature l2)