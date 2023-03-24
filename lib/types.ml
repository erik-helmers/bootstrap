type atom = Atom.t
type 'a binder = 'a Binder.t

let equal_atom = Atom.equal
let equal_binder = Binder.equal

type label = string [@@deriving eq]

type term =
  | Free of atom
  | Bound of int
  | Lam of term binder
  | App of term * term
  | Pi of term * term binder
  | Tuple of term * term
  | Fst of term
  | Snd of term
  | Sigma of term * term binder
  | Annot of term * term
  | Star
  | Unit
  | Nil
  | LabelTy
  | Label of label
  | LabelsTy
  | NilL
  | ConsL of term * term
  | Enum of term
  | EnumZe
  | EnumSuc of term
  | Record of term * term binder
  | Case of term * term binder * term
  | DUnit
  | DVar
  | DPi of term * term
  | DSigma of term * term
  | Decode of term * term
  | DescTy
[@@deriving eq]

type value =
  | VNeu of neutral
  | VLam of (value -> value)
  | VPi of value * (value -> value)
  | VTuple of value * value
  | VSigma of value * (value -> value)
  | VStar
  | VUnit
  | VNil
  | VLabelTy
  | VLabel of string
  | VLabelsTy
  | VNilL
  | VConsL of value * value
  | VEnum of value
  | VEnumZe
  | VEnumSuc of value
  | VDUnit
  | VDVar
  | VDPi of value * value
  | VDSigma of value * value
  | VDescTy

and neutral =
  | NVar of atom
  | NApp of neutral * value
  | NFst of neutral
  | NSnd of neutral
  | NRecord of neutral * (value -> value)
  | NCase of neutral * (value -> value) * value
  | NDecode of neutral * value

let traverse map_free map_bound term =
  let rec aux i term =
    match term with
    | Free a -> ( match map_free i a with Some t -> t | _ -> term)
    | Bound j -> ( match map_bound i j with Some t -> t | _ -> term)
    | Lam b -> Lam (Binder.weaken aux i b)
    | App (t, t') -> App (aux i t, aux i t')
    | Pi (t, b) -> Pi (aux i t, Binder.weaken aux i b)
    | Tuple (t, t') -> Tuple (aux i t, aux i t')
    | Fst t -> Fst (aux i t)
    | Snd t -> Snd (aux i t)
    | Sigma (t, b) -> Pi (aux i t, Binder.weaken aux i b)
    | Annot (x, t) -> Annot (aux i x, aux i t)
    | Star -> Star
    | Unit -> Unit
    | Nil -> Nil
    | LabelTy -> LabelTy
    | Label s -> Label s
    | LabelsTy -> LabelsTy
    | NilL -> NilL
    | ConsL (l, ls) -> ConsL (aux i l, aux i ls)
    | Enum t -> Enum (aux i t)
    | EnumZe -> EnumZe
    | EnumSuc t -> EnumSuc (aux i t)
    | Record (t, t') -> Record (aux i t, Binder.weaken aux i t')
    | Case (e, t, cs) -> Case (aux i e, Binder.weaken aux i t, aux i cs)
    | DUnit -> DUnit
    | DVar -> DVar
    | DPi (t, t') -> DPi (aux i t, aux i t')
    | DSigma (t, t') -> DSigma (aux i t, aux i t')
    | Decode (t, t') -> Decode (aux i t, aux i t')
    | DescTy -> DescTy
  in
  aux 0 term

(* Substitues Bound 0 for v in s  *)
let scoped_bind v s =
  traverse (fun _ _ -> None) (fun i j -> if i = j then Some v else None) s

(* Substitues Free a for Bound 0 in s  *)
let scoped_unbind a t =
  traverse
    (fun i b -> if Atom.equal a b then Some (Bound i) else None)
    (fun _ _ -> None)
    t

let open_ = Binder.open_ (fun a -> Free a |> scoped_bind)
let close_ = Binder.close_ scoped_unbind

exception BadTerm of string * term
exception BadValue of string * value

let bad_value s v = BadValue (s, v)
let bad_term s v = BadTerm (s, v)
