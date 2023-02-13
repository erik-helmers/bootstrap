type atom = Atom.t

type expr =
  | EFree of name
  | EBound of int
  | EApp of expr * expr
  | EFun of expr
  | ETuple of expr * expr
  | EAnnot of expr * expr
  | EStar
  | EPi of expr * expr
  | ESig of expr * expr
[@@deriving show]

and name = Global of string | Local of int | Quote of int
[@@deriving show]

type value =
  | Neu of neutral
  | Lam of (value -> value)
  | Tuple of value * value
  | Star
  | Pi of value * (value -> value)
  | Sig of value * (value -> value)

and neutral = NFree of name | NApp of neutral * value

let vfree n = Neu (NFree n)
let vvar x = vfree (Global x)
let global id = EFree (Global id)
