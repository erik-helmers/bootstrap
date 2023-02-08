open Syntax
module Ctx = Context

exception TypeError of value

let rec interpret ctx expr =
  match expr with
  | EStar -> Star
  | EPi (r, r') ->
      let t = interpret ctx r in
      let t' v = interpret (Ctx.push ctx v) r' in
      Pi (t, t')
  | ESig (r, r') ->
      let t = interpret ctx r in
      let t' v = interpret (Ctx.push ctx v) r' in
      Sig (t, t')
  | EBound id -> Ctx.bound ctx id
  | EFree name -> Ctx.name_val ctx name
  | EFun body ->
      Lam (fun arg -> interpret (Ctx.push ctx arg) body)
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
    | Pi (v, f) -> EPi (aux i v, aux (i + 1) (f (vvar name)))
    | Sig (v, f) -> ESig (aux i v, aux (i + 1) (f (vvar name)))
    | Lam f -> EFun ( aux (i + 1) (f (vvar name)))
    | Neu (NVar id) -> EFree (Global id)
    | Neu (NApp (n, value)) -> EApp (aux i (Neu n), aux i value)
    | Tuple (v1, v2) -> ETuple (aux i v1, aux i v2)
  in
  aux 0
