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
    Int of int (* in place of neutral terms *)
  | Lam of (value -> value)
;;

let fold_args args body =
  List.fold_right (fun arg acc -> EFun(arg,acc)) args body
