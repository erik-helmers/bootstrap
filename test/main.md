
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
# pi "x" star (fun x -> x);;
- : term = Pi (Star, {name = "x"; scoped = Bound 0})
# sigma "x" star (fun x -> x);;
- : term = Sigma (Star, {name = "x"; scoped = Bound 0})
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
# fn "x" (fun x -> x);;
- : term = (fn x -> x)
# app (var "f") (var "x");;
- : term = (f x)
# pi "x" (var "*") (fun x -> x);;
- : term = Π(x : *).x
# tuple (var "x", var "y");;
- : term = (x, y)
# first (var "t");;
- : term = (fst t)
# second (var "t");;
- : term = (snd t)
# sigma  "x" (var "*") (fun x -> x);;
- : term = Σ(x : *).x
# annot (var "x") (var "t");;
- : term = (x : t)
# star;;
- : term = *
# unit;;
- : term = unit
# nil;;
- : term = nil
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
- : term = (n (fn q0 -> q0))
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
- : term = (fst n)
# quote (VNeu (NSnd n));;
- : term = (snd n)
```


# Norm

```ocaml
# open Norm;;
# let id = fn "x" (fun x -> x);;
val id : term = (fn x -> x)
# norm id;;
- : term = (fn q0 -> q0)
# let pair = fn "x" (fun x -> tuple(x,x));;
val pair : term = (fn x -> (x, x))
# norm pair;;
- : term = (fn q0 -> (q0, q0))
# let weird_id = fn "x" (fun x -> first (app pair x));;
val weird_id : term = (fn x -> (fst ((fn x -> (x, x)) x)))
# norm weird_id;;
- : term = (fn q0 -> q0)
# equal_term id weird_id;;
- : bool = false
# norm_equal id weird_id;;
- : bool = true
```


# Typing 

We are now pretty confident with the `interpret` and `quote` functions, let's add handy operators for them.
```ocaml
# let (?$) = interpret;;
val ( ?$ ) : term -> value = <fun>
# let (?:) = quote;;
val ( ?: ) : value -> term = <fun>
```


And here are a few helpers
```ocaml
# open Typing;;
# let ctx = ref Ctx.empty;;
val ctx : '_weak1 Map.t ref = {contents = <abstr>}
# let assume a v = ctx := Ctx.add a v !ctx;; 
val assume : atom -> '_weak1 -> unit = <fun>
# let check t ty = check !ctx t ty;;
val check : term -> value -> unit = <fun>
# let synth t = synth !ctx t;;
val synth : term -> value = <fun>
```

## Misc

```ocaml
# check star ?$star;;
- : unit = ()
# check star ?$bool_ty;;
Exception:
Scratch.Typing.Mismatch {expected = (Enum {'false 'true}); got = *}.
```

## Pi

First we check the `(Lam)` rule, then `(App)` with and without neutral values.

```ocaml
# let aint, ax, ay = atom "int", atom "x", atom "y";; 
val aint : atom = int
val ax : atom = x
val ay : atom = y
# let int, x, y = avar aint, avar ax, avar ay;; 
val int : term = int
val x : term = x
val y : term = y
# assume aint ?$star; assume ax ?$int; assume ay ?$ int;; 
- : unit = ()
# let int_id = fn "x" (fun x -> x);;
val int_id : term = (fn x -> x)
# let int_id_ty = pi "_" int (fun _ -> int);;
val int_id_ty : term = Π(_ : int).int
# let int_id_vty = interpret int_id_ty;;
val int_id_vty : value = VPi (VNeu (NVar int), <fun>)
# check int_id int_id_vty;;
- : unit = ()
# check int_id ?$(pi "_" int (fun _ -> bool_ty));;
Exception:
Scratch.Typing.Mismatch {expected = (Enum {'false 'true}); got = int}.
# synth (app (annot int_id int_id_ty) (x));;
- : value = VNeu (NVar int)
# let f =  atom "f";;
val f : atom = f
# assume f int_id_vty;;
- : unit = ()
# synth (app (avar f) x);;
- : value = VNeu (NVar int)
```

Then we have the `(Pi)` rule.

```ocaml
# check (pi "_" int (fun _ -> int)) ?$star;;
- : unit = ()
```


## Booleans 

Here are the happy case, and the three sad cases (bad condition, type or branch type).

```ocaml
# let t x = cond x star bool_ty star;;
val t : term -> term = <fun>
# check (condition true_ "x" t true_ bool_ty)  ?$(t true_);;
- : unit = ()
# check (condition star "x" t true_ bool_ty )  ?$(t true_);;
Exception:
Scratch.Types.BadTerm ("synth : term type is not synthetisable", *).
# check (condition true_ "x" (fun _ -> true_) true_ bool_ty )  ?$(t true_);;
Exception:
Scratch.Typing.Mismatch {expected = *; got = (Enum {'false 'true})}.
# check (condition true_ "x" t star bool_ty) ?$(t true_);;
Exception:
Scratch.Typing.Mismatch {expected = (Enum {'false 'true}); got = *}.
```

## Sigma 

Now let's test the `(Tuple)` rule, then `(Fst)` and `(Snd)` with and without neutral values.

```ocaml
# let int_pair = tuple (x, y);;
val int_pair : term = (x, y)
# let int_pair_ty = sigma "_" int (fun _ -> int);;
val int_pair_ty : term = Σ(_ : int).int
# let int_pair_vty = interpret int_pair_ty;;
val int_pair_vty : value = VSigma (VNeu (NVar int), <fun>)
# check int_pair int_pair_vty;;
- : unit = ()
# synth (first (annot int_pair int_pair_ty));;
- : value = VNeu (NVar int)
# synth (second (annot int_pair int_pair_ty));;
- : value = VNeu (NVar int)
```

Then we have the `(Sigma)` rule.

```ocaml
# check (sigma "_" int (fun _ -> int)) ?$star;;
- : unit = ()
# check (sigma "_" star (fun a -> a)) ?$star;;
- : unit = ()
```


## Some more

```ocaml
# let either x = cond x star int bool_ty;;
val either : term -> term = <fun>
# let f = fn "a" (fun a -> condition a "a" either x true_) ;;
val f : term = (fn a ->
    case a as a
    return
      case a as _ return * with ((Enum {'false 'true}), (int, nil))
    with (((1+ 0) : (Enum {'false 'true})), (x, nil)))
# let fty = pi "x" bool_ty either;;
val fty : term = Π(x : (Enum {'false 'true})).
    case x as _ return * with ((Enum {'false 'true}), (int, nil))
# check f ?$fty;;
- : unit = ()
# synth (app (annot f fty) true_);;
- : value = VNeu (NVar int)
# synth (app (annot f fty) false_);;
- : value = VEnum (VConsL (VLabel "true", VConsL (VLabel "false", VNilL)))
```
    
## Unit and nil

```ocaml
# check unit ?$star;;
- : unit = ()
# check nil ?$unit;;
- : unit = ()
```

## Labels 

```ocaml
# check label_ty ?$star;;
- : unit = ()
# check (label "toto") ?$label_ty;;
- : unit = ()
# check labels_ty ?$star;;
- : unit = ()
# check (labels []) ?$labels_ty;;
- : unit = ()
# check (labels [label "toto"; label "tata"]) ?$labels_ty;;
- : unit = ()
# check (labels [true_]) ?$labels_ty;;
Exception:
Scratch.Typing.Mismatch {expected = label; got = (Enum {'false 'true})}.
```


## Enums 

```ocaml
# let ltrue, lfalse = label "true", label "false";;
val ltrue : term = 'true
val lfalse : term = 'false
# let lbool = labels [ltrue; lfalse];;
val lbool : term = {'true 'false}
# let ebool = enum lbool;;
val ebool : term = (Enum {'true 'false})
# check ebool ?$star;;
- : unit = ()
# check (enum_idx 0) ?$ebool;;
- : unit = ()
# check (enum_idx 1) ?$ebool;;
- : unit = ()
# check (enum_idx 2) ?$ebool;;
Exception: Scratch.Typing.Mismatch {expected = (Enum {}); got = 0}.
# check (enum_idx 0) ?$(enum @@ labels []);;
Exception: Scratch.Typing.Mismatch {expected = (Enum {}); got = 0}.
# check (enum_idx 0) ?$(enum @@ labels []);;
Exception: Scratch.Typing.Mismatch {expected = (Enum {}); got = 0}.
```

## Record and switch

```ocaml
# ebool;;
- : term = (Enum {'true 'false})
# let ty = norm (record (labels []) "_" (fun _ -> bool_ty));;
val ty : term = unit
# let ty = norm (record lbool "_" (fun _ -> bool_ty));;
val ty : term = Σ(q0 : (Enum {'false 'true})).Π(q1 : (Enum {'false
  'true})).unit
# let cs = tuple (true_, tuple (false_, nil));;
val cs : term = (
    ((1+ 0) : (Enum {'false 'true})),
    ((0 : (Enum {'false 'true})), nil)
  )
# ?$(record lbool "_" (fun _ -> bool_ty));;
- : value =
VSigma (VEnum (VConsL (VLabel "true", VConsL (VLabel "false", VNilL))),
 <fun>)
# check cs ?$(record lbool "_" (fun _ -> bool_ty)) ;;
- : unit = ()
# ?$(case (enum_idx 0) "_" (fun _ -> Star) cs) ;;
- : value = VEnumSuc VEnumZe
# ?$(case (enum_idx 1) "_" (fun _ -> Star) cs) ;;
- : value = VEnumZe
# ?$(case (enum_idx 2) "_" (fun _ -> Star) cs) ;;
Exception:
Scratch.Types.BadTerm ("interpret: value is not a tuple", (snd
   (snd
     (
       ((1+ 0) : (Enum {'false 'true})),
       ((0 : (Enum {'false 'true})), nil)
     )))).
```


# Descriptions 

```ocaml
# norm @@ decode dunit int;;
- : term = unit
# norm @@ decode dvar int;;
- : term = int
# let ty = norm (decode (dpi bool_ty (fn "_" (fun _ -> dvar))) int);;
val ty : term = Π(q0 : (Enum {'false 'true})).int
# check (fn "c" (fun c -> cond c int x y)) ?$ty;;
- : unit = ()
# let ty = norm (decode (dsigma bool_ty (fn "_" (fun _ -> dvar))) int);;
val ty : term = Σ(q0 : (Enum {'false 'true})).int
# check (tuple (true_, x)) ?$ty;;
- : unit = ()

# let ty = norm (decode (dsigma bool_ty (fn "x" (fun x -> cond x desc_ty dvar dunit))) int);;
val ty : term = Σ(q0 : (Enum {'false 'true})).
    ⟦case q0 as q1 return desc with (`unit, (`var, nil))⟧ (int)
# check (tuple(true_, x)) ?$ty;;
- : unit = ()
# check (tuple(true_, nil)) ?$ty;;
Exception: Scratch.Typing.Mismatch {expected = int; got = unit}.
# check (tuple(false_, nil)) ?$ty;;
- : unit = ()
```

