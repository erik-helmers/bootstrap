
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
- : term binder = {name = "x"; scoped = Scoped (Bound 0)}
# binder "x" (fun x -> var "x");;
- : term binder = {name = "x"; scoped = Scoped (Free {uid = 3; name = "x"})}
```

Functions : 

```ocaml
# Atom.global_reset ();;
- : unit = ()
# fn "x" (fun x -> x);;
- : term = Lam {name = "x"; scoped = Scoped (Bound 0)}
# fn2 "x" "y" (fun x y -> x);;
- : term =
Lam
 {name = "x"; scoped = Scoped (Lam {name = "y"; scoped = Scoped (Bound 1)})}
# fn3 "x" "y" "z" (fun x y z -> x);;
- : term =
Lam
 {name = "x";
  scoped =
   Scoped
    (Lam
      {name = "y";
       scoped = Scoped (Lam {name = "z"; scoped = Scoped (Bound 2)})})}
```

Pi and sigma terms : 
```ocaml
# Atom.global_reset ();;
- : unit = ()
# let star = var "*";;
val star : term = Free {uid = 1; name = "*"}
# pi "x" star (fun x -> x);;
- : term =
Pi (Free {uid = 1; name = "*"}, {name = "x"; scoped = Scoped (Bound 0)})
# sigma "x" star (fun x -> x);;
- : term =
Pi (Free {uid = 1; name = "*"}, {name = "x"; scoped = Scoped (Bound 0)})
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
# Atom.global_reset ();;
- : unit = ()
# Bound 0;;
- : term = [0]
# let x = atom "x";;
val x : atom = x@1
# Free x;;
- : term = x@1
# fn "x" (fun x -> x);;
- : term = (fun x@3 -> x@3)
# pi "x" (var "*") (fun x -> x);;
- : term = Π(x@6 : *@4).x@6
# sigma  "x" (var "*") (fun x -> x);;
- : term = Π(x@9 : *@7).x@9
```
# Types
## Bindings
         
### Bind
    
```ocaml
# Atom.global_reset();;
- : unit = ()
# let id = binder "x" (fun x -> x);;
val id : term binder = {name = "x"; scoped = Scoped [0]}
# let cons = binder "x" (fun x -> fn "y" (fun y -> x));;
val cons : term binder = {name = "x"; scoped = Scoped (fun y@4 -> [1])}
# bind id (var "a");;
- : term = a@5
# bind cons (var "b");;
- : term = (fun y@7 -> b@6)
```

### Open

```ocaml
# Atom.global_reset();;
- : unit = ()
# let id = binder "x" (fun x -> x);;
val id : term binder = {name = "x"; scoped = Scoped [0]}
# let cons = binder "x" (fun x -> fn "y" (fun y -> x));;
val cons : term binder = {name = "x"; scoped = Scoped (fun y@4 -> [1])}
# open_ id;;
- : atom * term = (x@5, x@5)
# open_ cons;;
- : atom * term = (x@6, (fun y@7 -> x@6))
```
### Close

```ocaml

# Atom.global_reset();;
- : unit = ()
# let x = atom "x";;
val x : atom = x@1
# let id = Free x;;
val id : term = x@1
# let cons = fn "y" (fun y -> Free x);;
val cons : term = (fun y@3 -> x@1)
# close x id;;
- : term binder = {name = "x"; scoped = Scoped [0]}
# close x cons;;
- : term binder = {name = "x"; scoped = Scoped (fun y@4 -> [1])}
```


