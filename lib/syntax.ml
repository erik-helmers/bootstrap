
type ident = string
[@@deriving show]

type expr =
    EIdent of ident
  | EApp of expr * expr
  | EFun of ident * expr
[@@deriving show]

let fold_args args body =
  List.fold_right (fun arg acc -> EFun(arg,acc)) args body
