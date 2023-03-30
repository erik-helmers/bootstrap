open Types

let binder name body =
  let atom = Atom.make name in
  close_ atom (body (Free atom))

let atom name = Atom.make name
let var name = Free (atom name)
let avar atom = Free atom

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
let label l = Label l
let labels_ty = LabelsTy
let consL l ls = ConsL (l, ls)
let nilL = NilL
let labels ls = List.fold_left (fun ls l -> consL l ls) NilL ls
let labelss ls = labels (List.map label ls)

(* Enums *)
let enum ls = Enum ls

let enum_idx i =
  let rec aux i acc =
    match i with 0 -> acc | _ -> aux (i - 1) (EnumSuc acc)
  in
  aux i EnumZe

(* Record and case *)
let record e x p = Record (e, binder x p)
let record_of_list ls = List.fold_right (fun t t' -> tuple (t, t')) ls Nil
let case e x p cs = Case (e, binder x p, cs)

(* Booleans *)

let bool_labels = labels (List.map label [ "false"; "true" ])
let bool_ty = enum bool_labels
let false_ = annot (enum_idx 0) bool_ty
let true_ = annot (enum_idx 1) bool_ty
let bool v = if v then true_ else false_
let condition c x t b b' = case c x t (tuple (b', tuple (b, nil)))

(* shorthand for uniform type in branches *)
let cond c t b b' = condition c "_" (fun _ -> t) b b'
(* Descriptions *)

let dunit = DUnit
let dvar = DVar
let dpi t t' = DPi (t, t')
let dsigma t t' = DSigma (t, t')
let decode d t = Decode (d, t)
let desc_ty = DescTy
let fix t = Fix t
let in_ t = In t
