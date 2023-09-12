# λT : A Toplevel designed for untyped lambda calculus

An implementation of untyped lambda calculus + a toplevel to manipulate terms.
100% done in Ocaml with no memory mutations, using de Bruijin indexes.
Every technical detail is precised in [Selinger's notes](https://ar5iv.labs.arxiv.org/html/0804.3434) 
You can find a proof that the beta reduction strategy used here is normalising in [Goubault's notes](http://www.lsv.fr/~goubault/Lambda/strategies_compressed.pdf)

## How to install 

You need to install dune before this. To install dune, follow [this tutorial](https://dune.readthedocs.io/en/stable/quick-start.html)
Then, download the installer, and run it. You are done !  
```
> git clone https://github.com/JeanGalte/lambda/
> cd lambda/LambdaTop
> dune build 
> cd  ..
```
And then, to start the program, just run 
```
./lambdatop
```

## How to use 

LambdaTop is a toplevel for untyped lambda calculus. Whenever you launch LambdaTop, you have an identifier, which you can see as a context for string<-> terms.  

To add a term to the identifier, use let command, let term_name = [term]. Term will be before capitalized, the identifier only contains term names starting with a capital.   

To remove a term from the identifier, use rm command, rm term_name, where term_name should be capitalized.  

To print identifier, use identifier command.  

To load the default identifier you can find in LambdaTop/lib/identifier.ml, use load_default_id  

To check if two terms are alpha_equivalent, use alpha_eq [term1] , [term2]  

To beta reduce a term, use beta command  

To print the whole beta reduction chain until normal form, if it exists under 1000000000 single steps, use print_beta_chain [term].

The beta reduction of a term is [beta term], the normal form is, if it exsits under 1000000000 single steps, [normalize term]. You can use terms registered in identifier, such as church integers with [] syntax. For example, with default identifier, 'Add[3][4]' will be parsed as (λλλ2(10))(λλ1(1(10)))(λλ1(1(1(10))))

## Features 

- [x] α-equivalence checker
- [x] β-reductor and β-equivalence checker
- [x] Proof for the β-reduction strategy 
- [x] Parser
- [x] Church integers 
- [x] Identifier : able to parse "Succ" as λn.λf.λx. n (λg.λh. h (g f)) (λu.x) (λu.u) for example
- [ ] Able to use other strategies (call by value, call by name)