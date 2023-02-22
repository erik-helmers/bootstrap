open Types

let binder name body =
  let atom = Atom.make name in
  close_ atom (body (Free atom))

let atom name = Atom.make name
let var name = Free (atom name)

(* Booleans *)
let bool v = Bool v
let cond c t t' = Cond (c, t, t')

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
