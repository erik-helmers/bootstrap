open Types

let var name = EFree (Atom.make name)

let binder name body =
  let atom = Atom.make name in
  closed atom (body (EFree atom))

let star = EStar
let annot e t = EAnnot (e, t)

(* Pi  *)
let fn name body = EFun (binder name body)
let fn2 x y body = fn x (fun x -> fn y (fun y -> body x y))
let fn3 x y z body = fn x (fun x -> fn2 y z (fun y z -> body x y z))
let app f x = EApp (f, x)
let pi name t body = EPi (t, binder name body)

(* Sigma *)
let tuple e1 e2 = ETuple (e1, e2)
let first e = failwith "todo"
let second e = failwith "todo"
let sigma t name body = ESig (t, binder name body)
