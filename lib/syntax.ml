type ident = string
[@@deriving show]


type top =
    TAssign of ident * expr
  | TAssume of ident * expr
[@@deriving show]
and expr =
    EIdent of ident
  | EApp of expr * expr
  | EFun of ident * expr
  | ETuple of expr * expr
  | EAnnot of expr * expr
  | EStar
  | EPi  of ident * expr * expr
  | ESig of ident * expr * expr
[@@deriving show]

type value =
    Neu of neutral
  | Lam of (value -> value)
  | Tuple of value * value
  | Star
  | Pi  of value * (value -> value)
  | Sig of value * (value -> value)
and neutral =
    NVar of ident
  | NApp of neutral * value
;;

let vvar x= Neu (NVar x)


let fold_args args body =
  List.fold_right (fun arg acc -> EFun(arg,acc)) args body
