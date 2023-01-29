open Syntax;;

module Ctx = Context;;

exception TypeError of value;;

let rec interpret ctx expr = match expr with
  | EIdent id -> Ctx.ident_val ctx id
  | EFun (id,body) -> Lam(fun arg ->
      interpret (Ctx.add_ident_val ctx id arg) body)
  | EApp(f, x) -> (let f = interpret ctx f in match f with
    | Lam f -> f (interpret ctx x)
    | Neu (NVar _ as id) -> Neu (NApp (id, interpret ctx x))
    | _ -> raise (TypeError f))
  | EAnnot (expr, _) -> interpret ctx expr
