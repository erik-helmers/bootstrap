open Types

let atom name = Atom.make name
let var name = Free (atom name)

let binder name body =
  let atom = Atom.make name in
  close atom (body (Free atom))

let fn x body = Lam (binder x body)
let fn2 x y body = fn x (fun x -> fn y (fun y -> body x y))
let fn3 x y z body = fn x (fun x -> fn2 y z (fun y z -> body x y z))
