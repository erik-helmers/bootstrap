open Syntax;;


type info =
    HasType of ty
  | HasKind of kind

type name = string;;

type t = {
    types: (name * info) list;
    values: (name * value) list;
};;

let empty () = { types = []; values = [] }

let base_ctx ={ values = [
  ];  types = [
  ]}

let ident_ty ctx ident = List.assoc ident ctx.types;;
let ident_ty_opt ctx ident = List.assoc_opt ident ctx.types;;

let add_ident_ty ctx ident ty = { ctx with types = (ident,ty)::ctx.types }
let pop_ident_ty ctx ident = { ctx with types = List.remove_assoc ident ctx.types }

let ident_val ctx ident = List.assoc ident ctx.values;;
let ident_val_opt ctx ident = List.assoc_opt ident ctx.values;;

let add_ident_val ctx ident value = { ctx with values = (ident,value)::ctx.values }
let pop_ident_val ctx ident = { ctx with values = List.remove_assoc ident ctx.values }
