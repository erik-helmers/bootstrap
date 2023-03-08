open Types

let binder name body =
  let atom = Atom.make name in
  close_ atom (body (Free atom))

let atom name = Atom.make name
let var name = Free (atom name)
let avar atom = Free atom

(* Booleans *)
let bool v = Bool v
let true_ = Bool true
let false_ = Bool false
let condition c x t b b' = Cond (c, binder x t, b, b')

(* shorthand for uniform type in branches *)
let cond c t b b' = Cond (c, binder "_" (fun _ -> t), b, b')
let bool_ty = BoolTy

(* Pi related terms  *)
let fn x body = Lam (binder x body)
let fn2 x y body = fn x (fun x -> fn y (fun y -> body x y))
let fn3 x y z body = fn x (fun x -> fn2 y z (fun y z -> body x y z))
let app f x = App (f, x)
let app2 f x y = App (app f x, y)
let app3 f x y z = App (app2 f x y, z)
let pi x t t' = Pi (t, binder x t')

(* Sigma related terms *)
let tuple (t, t') = Tuple (t, t')
let first t = Fst t
let second t = Snd t
let sigma x t t' = Sigma (t, binder x t')

(* Typing *)
let annot x t = Annot (x, t)
let star = Star

(* Unit and nil *)
let unit = Unit
let nil = Nil

(* Labels *)
let label_ty = LabelTy
let label s = Label s
