
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
# cond (var "x") (var "*") (var "y") (var "z");;
- : term = cond x [_ *] y z
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

# Interpret 

## Pi 

```ocaml
# open Interpret;;
# let id = fn "x" (fun x -> x);;
val id : term = (fn x -> x)
# let cons = fn2 "x" "y" (fun x y -> x);;
val cons : term = (fn x -> (fn y -> x))
# interpret id;;
- : value = VLam <fun>
# interpret (app id (var "a"));;
- : value = VNeu (NVar a)
# interpret (app2 cons (var "b") (var "c"));;
- : value = VNeu (NVar b)
# interpret (app (var "f") (var "x"));;
- : value = VNeu (NApp (NVar f, VNeu (NVar x)))
# interpret (pi "x" (var "star") (fun x -> x));;
- : value = VPi (VNeu (NVar star), <fun>)
```

## Sigma 
```ocaml
# let t = tuple (var "x",var "y");;
val t : term = (x, y)
# interpret t ;;
- : value = VTuple (VNeu (NVar x), VNeu (NVar y))
# interpret (first (var "t"));;
- : value = VNeu (NFst (NVar t))
# interpret (second (var "t"));;
- : value = VNeu (NSnd (NVar t))
# interpret (first t);;
- : value = VNeu (NVar x)
# interpret (second t);;
- : value = VNeu (NVar y)
# interpret (sigma "x" (var "star") (fun x -> x));;
- : value = VSigma (VNeu (NVar star), <fun>)
```

# Quote 

## Pi 

```ocaml
# open Quote;;
# let n = NVar (atom "n");;
val n : neutral = NVar n
# let id = VLam(fun x -> x);;
val id : value = VLam <fun>
# let cons = VLam(fun x -> VLam (fun y -> x));;
val cons : value = VLam <fun>
# quote id;;
- : term = (fn q0 -> q0)
# quote cons;;
- : term = (fn q0 -> (fn q1 -> q0))
# quote (VNeu (NApp(n,id)));;
- : term = n
(fn q0 -> q0)
```

## Sigma 
```ocaml
# let t = VTuple(id, cons);;
val t : value = VTuple (VLam <fun>, VLam <fun>)
# let n = NVar (atom "n");;
val n : neutral = NVar n
# quote t ;;
- : term = ((fn q0 -> q0), (fn q0 -> (fn q1 -> q0)))
# quote (VNeu (NFst n));;
- : term = fst
n
# quote (VNeu (NSnd n));;
- : term = snd
n
```
