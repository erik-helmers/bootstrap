open Syntax

type info = value
type name = string
type t = { types : (name * info) list; values : (name * value) list }

let empty = { types = []; values = [] }
let base_ctx = { values = []; types = [] }

let ident_ty ctx ident =
  try List.assoc ident ctx.types
  with Not_found -> failwith ("context: missing ident ty" ^ ident)

let ident_ty_opt ctx ident = List.assoc_opt ident ctx.types
let add_ident_ty ctx ident ty = { ctx with types = (ident, ty) :: ctx.types }

let pop_ident_ty ctx ident =
  { ctx with types = List.remove_assoc ident ctx.types }

let ident_val ctx ident =
  try List.assoc ident ctx.values
  with Not_found -> failwith ("context: missing ident val " ^ ident)

let ident_val_opt ctx ident = List.assoc_opt ident ctx.values

let add_ident_val ctx ident value =
  { ctx with values = (ident, value) :: ctx.values }

let pop_ident_val ctx ident =
  { ctx with values = List.remove_assoc ident ctx.values }

let add_ident ctx ident value ty =
  let ctx = add_ident_val ctx ident value in
  let ctx = add_ident_ty ctx ident ty in
  ctx

let pop_ident ctx ident =
  let ctx = pop_ident_val ctx ident in
  let ctx = pop_ident_ty ctx ident in
  ctx
