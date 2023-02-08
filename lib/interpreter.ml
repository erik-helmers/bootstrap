open Syntax
module Ctx = Context

exception TypeError of value

let rec interpret ctx expr =
  match expr with
  | EStar -> Star
  | EPi (id, r, r') ->
      let t = interpret ctx r in
      let t' v = interpret (Ctx.add_ident ctx id v t) r' in
      Pi (t, t')
  | ESig (id, r, r') ->
      let t = interpret ctx r in
      let t' v = interpret (Ctx.add_ident ctx id v t) r' in
      Sig (t, t')
  | EBound id -> failwith "todo"
  | EIdent id -> Ctx.ident_val ctx id
  | EFun (id, body) ->
      Lam (fun arg -> interpret (Ctx.add_ident_val ctx id arg) body)
  | EApp (f, x) -> (
      let f = interpret ctx f in
      match f with
      | Lam f -> f (interpret ctx x)
      | Neu (NVar _ as id) -> Neu (NApp (id, interpret ctx x))
      | _ -> raise (TypeError f))
  | ETuple (e1, e2) -> Tuple (interpret ctx e1, interpret ctx e2)
  | EAnnot (expr, _) -> interpret ctx expr

let quote =
  let rec aux i value =
    let name = "quote_" ^ string_of_int i in
    match value with
    | Star -> EStar
    | Pi (v, f) -> EPi (name, aux i v, aux (i + 1) (f (vvar name)))
    | Sig (v, f) -> ESig (name, aux i v, aux (i + 1) (f (vvar name)))
    | Neu (NVar id) -> EIdent id
    | Neu (NApp (n, value)) -> EApp (aux i (Neu n), aux i value)
    | Lam f -> EFun (name, aux (i + 1) (f (vvar name)))
    | Tuple (v1, v2) -> ETuple (aux i v1, aux i v2)
  in
  aux 0
