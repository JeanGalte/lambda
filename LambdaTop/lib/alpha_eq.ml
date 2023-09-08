open Identifier
open Parser

let alpha_eq (l1 : string) (l2 : string) (i : identifier) : bool = ((parse i l1) = (parse i l2))