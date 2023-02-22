```ocaml
# #warnings "-8";;
# open Scratch;;
# open Interpret;;
# open Quote;;
# open Norm;;
# open Types;;
# open Syntax;;
# #install_printer Pp.pp_term;;
# #install_printer Pp.pp_atom;;
```

# Church numerals

In this file, we test our library by defining Church numerals and
implementing some basic arithmetic operations on them.

## Definition

First, let us define `num i` which constructs the term representing `i`, Church encoded.

```ocaml
# let nseq n = List.init n (fun i -> i);;
val nseq : int -> int list = <fun>
# let num n = fn2 "f" "x" ( fun f x -> 
    List.fold_left (fun x _ -> app f x) x (nseq n)) ;;
val num : int -> term = <fun>
```


```ocaml
# num 0;;
- : term = (fn f -> (fn x -> x))
# num 1;;
- : term = (fn f -> (fn x -> (f x)))
# num 2;;
- : term = (fn f -> (fn x -> (f (f x))))
```


## Addition 

We can now define addition of two numerals.

```ocaml
# let add = fn2 "m" "n" (fun m n ->
     fn2 "f" "x" (fun f x -> app2 m f (app2 n f x)));;
val add : term = (fn m -> (fn n -> (fn f -> (fn x -> ((m f) ((n f) x))))))
```

```ocaml
# norm @@ app2 add (num 0) (num 0);;
- : term = (fn q0 -> (fn q1 -> q1))
# norm @@ app2 add (num 1) (num 2);;
- : term = (fn q0 -> (fn q1 -> (q0 (q0 (q0 q1)))))
# norm @@ app2 add (num 2) (num 1);;
- : term = (fn q0 -> (fn q1 -> (q0 (q0 (q0 q1)))))

# let correct a b = norm_eq (app2 add (num a) (num b)) (num (a+b));;
val correct : int -> int -> bool = <fun>
# correct 0 0;;
- : bool = true
# correct 0 1;;
- : bool = true
# correct 1 0;;
- : bool = true
# correct 1 1;;
- : bool = true
# correct 5 3;;
- : bool = true
# correct 10 10;;
- : bool = true
```


## Multiplication

```ocaml

# let mul = fn2 "m" "n" (fun m n ->
     fn2 "f" "x" (fun f x -> app2 m (app n f) x));;
val mul : term = (fn m -> (fn n -> (fn f -> (fn x -> ((m (n f)) x)))))
```

```ocaml
# app2 mul (num 0) (num 0);;
- : term = (((fn m -> (fn n -> (fn f -> (fn x -> ((m (n f)) x)))))
(fn f -> (fn x -> x)))
(fn f -> (fn x -> x)))
# app2 mul (num 1) (num 0);;
- : term = (((fn m -> (fn n -> (fn f -> (fn x -> ((m (n f)) x)))))
(fn f -> (fn x -> (f x))))
(fn f -> (fn x -> x)))
# app2 mul (num 1) (num 1);;
- : term = (((fn m -> (fn n -> (fn f -> (fn x -> ((m (n f)) x)))))
(fn f -> (fn x -> (f x))))
(fn f -> (fn x -> (f x))))
# app2 mul (num 2) (num 1);;
- : term = (((fn m -> (fn n -> (fn f -> (fn x -> ((m (n f)) x)))))
(fn f -> (fn x -> (f (f x)))))
(fn f -> (fn x -> (f x))))

# let correct a b = norm_eq (app2 mul (num a) (num b)) (num (a*b));;
val correct : int -> int -> bool = <fun>
# correct 0 0;;
- : bool = true
# correct 0 1;;
- : bool = true
# correct 1 0;;
- : bool = true
# correct 1 1;;
- : bool = true
# correct 5 3;;
- : bool = true
# correct 10 10;;
- : bool = true
```

## Exponentiation

```ocaml
# let exp = fn2 "m" "n" (fun m n ->
     fn2 "f" "x" (fun f x -> app3 n m f x));;
val exp : term = (fn m -> (fn n -> (fn f -> (fn x -> (((n m) f) x)))))
```

```ocaml
# app2 exp (num 1) (num 0);;
- : term = (((fn m -> (fn n -> (fn f -> (fn x -> (((n m) f) x)))))
(fn f -> (fn x -> (f x))))
(fn f -> (fn x -> x)))
# norm @@ app2 exp (num 1) (num 0);;
- : term = (fn q0 -> (fn q1 -> (q0 q1)))
# norm_eq (app2 exp (num 2) (num 3)) (num 8);;
- : bool = true
# norm_eq (app2 exp (num 2) (num 8)) (num 256);;
- : bool = true
```
