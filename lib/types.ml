let pp_atom = Atom.pp
and pp_binder = Binder.pp

type atom = Atom.t
and 'a binder = 'a Binder.t

type expr =
  | EFree of atom
  | EBound of int
  | EApp of expr * expr
  | ETuple of expr * expr
  | EAnnot of expr * expr
  | EStar
  | EFun of expr binder
  | EPi of expr * expr binder
  | ESig of expr * expr binder
[@@deriving show]

type value =
  | Neu of neutral
  | Lam of (value -> value)
  | Tuple of value * value
  | Star
  | Pi of value * (value -> value)
  | Sig of value * (value -> value)

and neutral = NFree of atom | NApp of neutral * value

let vfree n = Neu (NFree n)

(** Traverse l'expression en comptant les binders,
  * et à chaque noeud n, si map d n est None, continue normalement
  * sinon remplace l'expression courrante *)
let subst_ (map : int -> expr -> expr option) expr =
  let rec aux i expr =
    match map i expr with
    | Some e -> e
    | None -> (
        match expr with
        | EStar | EBound _ | EFree _ -> expr
        | EApp (a, b) -> EApp (aux i a, aux i b)
        | ETuple (a, b) -> ETuple (aux i a, aux i b)
        | EAnnot (a, b) -> EAnnot (aux i a, aux i b)
        | EFun f -> EFun (Binder.make f.name @@ aux (i + 1) f.scoped)
        | EPi (t, f) ->
            EPi (aux i t, Binder.make f.name @@ aux (i + 1) f.scoped)
        | ESig (t, f) ->
            ESig (aux i t, Binder.make f.name @@ aux (i + 1) f.scoped))
  in
  aux 0 expr

(* bind (release_bound) : transforme EBound 0 (profondeur ajustée) en v*)
let bind v = subst_ (fun i e -> if e = EBound i then Some v else None)

(* unbind (bind_free) : transforme un EFree en EBound 0 (à profondeur ajustée) *)
let unbind atom =
  subst_ (fun i e -> if e = EFree atom then Some (EBound i) else None)

let opened binder = Binder.opened (fun a -> bind (EFree a)) binder
let closed atom body = Binder.closed unbind atom body
