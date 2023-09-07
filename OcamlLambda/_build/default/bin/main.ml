open OcamlLambda
open Identifier
open Parser
open External

let () = print_lambda (parse default_identifier "x(yz)Omega") default_identifier 
