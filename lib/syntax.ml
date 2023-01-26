type ident = string
[@@deriving show]

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

let fold_args args body =
  List.fold_right (fun arg acc -> EFun(arg,acc)) args body
