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
  | EFree name -> (
      match Ctx.name_ty_opt ctx name with
      | Some ty -> ty
      | _ -> failwith "typing error: unknown ident")
  | EApp (e1, e2) -> (
      match typeof ctx e1 with
      | Pi (t, t') ->
          check ctx e2 t;
          t' (interpret ctx e2)
      | _ -> failwith "typing error: type is not applicable ")
  | EStar -> Star
  | EPi ( r, r') ->
      check ctx r Star;
      let ty = interpret ctx r in
      check (Ctx.add_name ctx (Global "fixme") (vvar "fixme") ty) r' Star;
      Star
  | ESig ( r, r') ->
      check ctx r Star;
      let ty = interpret ctx r in
      check (Ctx.add_name ctx (Global "fixme") (vvar "fixme") ty) r' Star;
      Star
  | _ -> failwith "typing: expr is not inferrable"

and check ctx expr ty =
  match expr with
  | EFun body -> (
      match ty with
      | Pi (t, t') -> check (Ctx.add_name_ty ctx (Global "fixme") t) body (t' (vvar "fixme"))
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
