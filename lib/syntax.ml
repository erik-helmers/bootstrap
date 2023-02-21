open Types

let atom name = Atom.make name
let var name = Free (atom name)
let bool v = Bool v

let binder name body =
  let atom = Atom.make name in
  close_ atom (body (Free atom))

let fn x body = Lam (binder x body)
let fn2 x y body = fn x (fun x -> fn y (fun y -> body x y))
let fn3 x y z body = fn x (fun x -> fn2 y z (fun y z -> body x y z))
let pi x t t' = Pi (t, binder x t')
let sigma x t t' = Pi (t, binder x t')
