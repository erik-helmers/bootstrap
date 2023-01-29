type ident = string
[@@deriving show]

type kind = Star;;

type ty =
    Ty of ident
  | TFun of ty * ty
[@@deriving show]

type expr =
    EIdent of ident
  | EApp of expr * expr
  | EFun of ident * expr
  | EAnnot of expr * ty
[@@deriving show]

type value =
    Neu of neutral
  | Lam of (value -> value)
and neutral =
    NVar of ident
  | NApp of neutral * value
;;

let vvar x= Neu (NVar x)


let fold_args args body =
  List.fold_right (fun arg acc -> EFun(arg,acc)) args body
