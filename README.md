# Ocaml implementation of untyped lambda calculus

Here is an implementation of the lambda untyped lambda calculus in ocaml, 100% side-effects free.  
Every technical detail is precised in [Selinger's notes](https://ar5iv.labs.arxiv.org/html/0804.3434) 

## Features 

- [x] α-equivalence checker
- [x] β-reductor and β-equivalence checker
- [ ] Proof for the β-reduction strategy 
- [ ] η-reductor,  η-β implementation
- [x] Lambda term printer, with identified terms printing
- [x] Parser
- [x] Church integers 
- [x] Identifier : able to parse "Succ" as λn.λf.λx. n (λg.λh. h (g f)) (λu.x) (λu.u) for example
- [ ] Identifier : able to extract an identifier from a file
