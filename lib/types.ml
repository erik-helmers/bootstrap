type atom = Atom.t
type 'a binder = 'a Binder.t

type term =
  | Free of atom
  | Bound of int
  | Bool of bool
  | Cond of term * term binder * term * term
  | Lam of term binder
  | App of term * term
  | Pi of term * term binder
  | Tuple of term * term
  | Fst of term
  | Snd of term
  | Sigma of term * term binder

type value =
  | VNeu of neutral
  | VBool of bool
  | VLam of (value -> value)
  | VPi of value * (value -> value)
  | VTuple of value * value
  | VSigma of value * (value -> value)

and neutral =
  | NVar of atom
  | NCond of neutral * (value -> value) * value * value
  | NApp of neutral * value
  | NFst of neutral
  | NSnd of neutral

let traverse map_free map_bound term =
  let rec aux i (term : term) =
    match term with
    | Free a -> ( match map_free i a with Some t -> t | _ -> term)
    | Bound j -> ( match map_bound i j with Some t -> t | _ -> term)
    | Bool _ -> term
    | Cond (c, t, b, b') ->
        Cond (aux i c, Binder.weaken aux i t, aux i b, aux i b')
    | Lam b -> Lam (Binder.weaken aux i b)
    | App (t, t') -> App (aux i t, aux i t')
    | Pi (t, b) -> Pi (aux i t, Binder.weaken aux i b)
    | Tuple (t, t') -> Tuple (aux i t, aux i t')
    | Fst t -> Fst (aux i t)
    | Snd t -> Snd (aux i t)
    | Sigma (t, b) -> Pi (aux i t, Binder.weaken aux i b)
  in
  aux 0 term

(* Substitues Bound 0 for v in s  *)
let scoped_bind v s =
  traverse (fun _ _ -> None) (fun i j -> if i = j then Some v else None) s

(* Substitues Free a for Bound 0 in s  *)
let scoped_unbind a t =
  traverse
    (fun i b -> if Atom.eq a b then Some (Bound i) else None)
    (fun _ _ -> None)
    t

let open_ = Binder.open_ (fun a -> Free a |> scoped_bind)
let close_ = Binder.close_ scoped_unbind
