
```ocaml
# #warnings "-8";;
# open Scratch;;
# open Atom;;
# open Binder;; 
# open Types;; 
# open Pp;;
# open Syntax;;
```

Ce fichier permet de rapidement prototyper des tests. 
  - `dune runtest` lance les tests
  - `dune promote` met à jour les sorties attendues

# Syntax functions

Just checking that the syntax functions are working as expected.

```ocaml
# binder "x" (fun x -> x);;
- : term binder = {name = "x"; scoped = Bound 0}
# binder "x" (fun x -> var "x");;
- : term binder = {name = "x"; scoped = Free {uid = 3; name = "x"}}
```

Functions : 

```ocaml
# fn "x" (fun x -> x);;
- : term = Lam {name = "x"; scoped = Bound 0}
# fn2 "x" "y" (fun x y -> x);;
- : term = Lam {name = "x"; scoped = Lam {name = "y"; scoped = Bound 1}}
# fn3 "x" "y" "z" (fun x y z -> x);;
- : term =
Lam
 {name = "x";
  scoped = Lam {name = "y"; scoped = Lam {name = "z"; scoped = Bound 2}}}
```

Pi and sigma terms : 
```ocaml
# let star = var "*";;
val star : term = Free {uid = 10; name = "*"}
# pi "x" star (fun x -> x);;
- : term = Pi (Free {uid = 10; name = "*"}, {name = "x"; scoped = Bound 0})
# sigma "x" star (fun x -> x);;
- : term =
Sigma (Free {uid = 10; name = "*"}, {name = "x"; scoped = Bound 0})
```
We can be pretty confident in our constructors now. Let's enable pretty printing.

```ocaml
# pp_term;;;
- : Format.formatter -> term -> unit = <fun>
# #install_printer pp_term;;
# #install_printer pp_atom;;
```

Here is a reminder of the format for different terms.
```ocaml
# Bound 0;;
- : term = [0]
# let x = atom "x";;
val x : atom = x
# Free x;;
- : term = x
# Bool true, Bool false;;
- : term * term = (true, false)
# fn "x" (fun x -> x);;
- : term = (fn x -> x)
# app (var "f") (var "x");;
- : term = f
x
# pi "x" (var "*") (fun x -> x);;
- : term = Π(x : *).x
# tuple (var "x", var "y");;
- : term = (x, y)
# first (var "t");;
- : term = fst
t
# second (var "t");;
- : term = snd
t
# sigma  "x" (var "*") (fun x -> x);;
- : term = Σ(x : *).x
```
# Types
## Bindings
         
### Bind
    
```ocaml
# let id = binder "x" (fun x -> x);;
val id : term binder = {name = "x"; scoped = [0]}
# let cons = binder "x" (fun x -> fn "y" (fun y -> x));;
val cons : term binder = {name = "x"; scoped = (fn y -> [1])}
# bind id (var "a");;
- : term = a
# bind cons (var "b");;
- : term = (fn y -> b)
```

### Open

```ocaml
# let id = binder "x" (fun x -> x);;
val id : term binder = {name = "x"; scoped = [0]}
# let cons = binder "x" (fun x -> fn "y" (fun y -> x));;
val cons : term binder = {name = "x"; scoped = (fn y -> [1])}
# open_ id;;
- : atom * term = (x, x)
# open_ cons;;
- : atom * term = (x, (fn y -> x))
```
### Close

```ocaml

# let x = atom "x";;
val x : atom = x
# let id = Free x;;
val id : term = x
# let cons = fn "y" (fun y -> Free x);;
val cons : term = (fn y -> x)
# close_ x id;;
- : term binder = {name = "x"; scoped = [0]}
# close_ x cons;;
- : term binder = {name = "x"; scoped = (fn y -> [1])}
```


