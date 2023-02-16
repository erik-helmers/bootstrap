open Types
module Ctx = Context

exception TypeError of value

let rec interpret ctx expr =
  match expr with
  | EStar -> Star
  | EBound id -> Ctx.bound ctx id
  | EFree name -> Ctx.name_val ctx name
  | EFun f -> Lam (fun arg -> interpret (Ctx.push ctx arg) f.scoped)
  | EPi (r, f) ->
      let t = interpret ctx r in
      let t' v = interpret (Ctx.push ctx v) f.scoped in
      Pi (t, t')
  | ESig _ -> failwith "todo"
  | EApp (f, x) -> (
      let f = interpret ctx f in
      match f with
      | Lam f -> f (interpret ctx x)
      | Neu (NFree _ as id) -> Neu (NApp (id, interpret ctx x))
      | _ -> raise (TypeError f))
  | ETuple (e1, e2) -> Tuple (interpret ctx e1, interpret ctx e2)
  | EAnnot (expr, _) -> interpret ctx expr

let quote expr =
  let rec aux i value =
    let name = Atom.Quote.make i in
    match value with
    | Star -> EStar
    | Pi (v, f) ->
        EPi (aux i v, closed name (aux (i + 1) @@ f (vfree name)))
    | Sig (v, f) -> failwith "todo"
    | Lam f -> EFun (closed name (aux (i + 1) @@ f (vfree name)))
    | Neu (NFree a) ->
        if Atom.Quote.is_quote a then EBound (i - Atom.Quote.depth a - 1)
        else EFree a
    | Neu (NApp (n, value)) -> EApp (aux i (Neu n), aux i value)
    | Tuple (v1, v2) -> ETuple (aux i v1, aux i v2)
  in
  aux 0 expr
