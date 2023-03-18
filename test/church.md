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

# let correct a b = norm_equal (app2 add (num a) (num b)) (num (a+b));;
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

# let correct a b = norm_equal (app2 mul (num a) (num b)) (num (a*b));;
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
# norm_equal (app2 exp (num 2) (num 3)) (num 8);;
- : bool = true
# norm_equal (app2 exp (num 2) (num 8)) (num 256);;
- : bool = true
```


# Church booleans

Let's now define Church booleans and compare their behavior with our `Bool` and `Cond` terms.

## Definition

```ocaml
# let ctrue = fn2 "a" "b" (fun a b -> a);;
val ctrue : term = (fn a -> (fn b -> a))
# let cfalse = fn2 "a" "b" (fun a b -> b);;
val cfalse : term = (fn a -> (fn b -> b))
# let cbool b = norm_equal b ctrue;; (* makes our lives easy *);;
val cbool : term -> bool = <fun>
```

```ocaml
# let _true = bool true;;
val _true : term = ((1+ 0) : (Enum {'false 'true}))
# let _false = bool false;;
val _false : term = (0 : (Enum {'false 'true}))
# let _bool b = norm_equal b _true;; 
val _bool : term -> bool = <fun>
```

## Operations 

```ocaml
# let cand = fn2 "p" "q" (fun p q -> app2 p q p);;
val cand : term = (fn p -> (fn q -> ((p q) p)))
# let cor  = fn2 "p" "q" (fun p q -> app2 p p q);;
val cor : term = (fn p -> (fn q -> ((p p) q)))
# let cnot = fn  "p" (fun p -> app2 p cfalse ctrue);;
val cnot : term = (fn p -> ((p (fn a -> (fn b -> b))) (fn a -> (fn b -> a))))
# let cif  = fn3 "p" "a" "b" (fun p a b -> app2 p a b);;
val cif : term = (fn p -> (fn a -> (fn b -> ((p a) b))))
```

```ocaml

# let test_binop op = List.map (fun (a,b) -> cbool (app2 op a b))
      [ cfalse, cfalse; cfalse, ctrue; ctrue, cfalse; ctrue, ctrue];;
val test_binop : term -> bool list = <fun>

# test_binop cand;;
- : bool list = [false; false; false; true]
# test_binop cor;;
- : bool list = [false; true; true; true]
```

And here are similar operations on our `Bool` and `Cond` terms.
```ocaml
# let bcond = let ty=var "bool" in fun c -> cond c ty;;
val bcond : term -> term -> term -> term = <fun>
# let tand = fn2 "p" "q" (fun p q -> 
    bcond p (bcond q _true _false)
             _false);;
val tand : term = (fn p ->
    (fn q ->
      case p as _ return bool
      with
        (
          (0 : (Enum {'false 'true})),
          (
            case q as _ return bool
            with
              (
                (0 : (Enum {'false 'true})),
                (((1+ 0) : (Enum {'false 'true})), nil)
              ),
            nil
          )
        )))
# let tor = fn2 "p" "q" (fun p q -> 
    bcond p _true
           (bcond q _true _false));;
val tor : term = (fn p ->
    (fn q ->
      case p as _ return bool
      with
        (
          case q as _ return bool
          with
            (
              (0 : (Enum {'false 'true})),
              (((1+ 0) : (Enum {'false 'true})), nil)
            ),
          (((1+ 0) : (Enum {'false 'true})), nil)
        )))
# let tnot = fn "p" (fun p -> bcond p _false _true);;
val tnot : term = (fn p ->
    case p as _ return bool
    with
      (
        ((1+ 0) : (Enum {'false 'true})),
        ((0 : (Enum {'false 'true})), nil)
      ))
```


```ocaml
# let test_binop op = List.map (fun (a,b) -> _bool (app2 op a b))
      [ _false, _false; _false, _true; _true, _false; _true, _true];;
val test_binop : term -> bool list = <fun>

# test_binop tand;;
- : bool list = [false; false; false; true]
# test_binop tor;;
- : bool list = [false; true; true; true]
```
