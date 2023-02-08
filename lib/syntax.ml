type ident = string [@@deriving show]

type top = TAssign of ident * expr | TAssume of ident * expr [@@deriving show]

and expr =
  | EFree of name
  | EBound of int
  | EApp of expr * expr
  | EFun of ident * expr
  | ETuple of expr * expr
  | EAnnot of expr * expr
  | EStar
  | EPi of ident * expr * expr
  | ESig of ident * expr * expr
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

and neutral = NVar of ident | NApp of neutral * value

let vvar x = Neu (NVar x)
let global id = EFree (Global id)

let rec subst_arg arg = let rec aux i expr = match expr with
  | EApp (f,x) -> EApp(aux i f, aux i x)
  | EFun (id,f) -> EFun(id, aux (i+1) f)
  | EPi (id,a,b) -> EPi(id,aux i a, aux (i+1) b)
  | ESig (id,a,b) -> ESig(id, aux i a, aux (i+1) b)
  | ETuple (a,b) -> ETuple (aux i a, aux i b)
  | EAnnot (a,b) -> EAnnot (aux i a, aux i b)
  | EFree Global id -> if id=arg then EBound i else expr
  | EFree _ -> failwith "unexpected"
  | EStar | EBound _ -> expr
in aux 0

let fold_args args body =
  List.fold_right (fun arg acc -> EFun (arg, subst_arg arg acc)) args body
