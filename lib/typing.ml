open Syntax
open Interpreter
module Ctx = Context

let rec typeof ctx expr =
  match expr with
  | EAnnot (expr, ty) ->
      check ctx ty Star;
      let ty = interpret ctx ty in
      check ctx expr ty;
      ty
  | EBound id -> failwith "todo"
  | EIdent id -> (
      match Ctx.ident_ty_opt ctx id with
      | Some ty -> ty
      | _ -> failwith "typing error: unknown ident")
  | EApp (e1, e2) -> (
      match typeof ctx e1 with
      | Pi (t, t') ->
          check ctx e2 t;
          t' (interpret ctx e2)
      | _ -> failwith "typing error: type is not applicable ")
  | EStar -> Star
  | EPi (id, r, r') ->
      check ctx r Star;
      let ty = interpret ctx r in
      check (Ctx.add_ident ctx id (vvar id) ty) r' Star;
      Star
  | ESig (id, r, r') ->
      check ctx r Star;
      let ty = interpret ctx r in
      check (Ctx.add_ident ctx id (vvar id) ty) r' Star;
      Star
  | _ -> failwith "typing: expr is not inferrable"

and check ctx expr ty =
  match expr with
  | EFun (id, body) -> (
      match ty with
      | Pi (t, t') -> check (Ctx.add_ident_ty ctx id t) body (t' (vvar id))
      | _ -> failwith "typing: ill-typed expr")
  | ETuple (e1, e2) -> (
      match ty with
      | Sig (t1, t2) ->
          check ctx e1 t1;
          check ctx e2 (t2 (interpret ctx e1))
      | _ -> failwith "typing: ill-typed expr")
  | _ ->
      if quote (typeof ctx expr) <> quote ty then
        failwith "typing: ill-typed expr"
