open Types
open Interpreter
module Ctx = Context

let rec typeof i ctx expr =
  match expr with
  | EAnnot (expr, ty) ->
      check i ctx ty Star;
      let ty = interpret ctx ty in
      check i ctx expr ty;
      ty
  | EFree name -> (
      match Ctx.name_ty_opt ctx name with
      | Some ty -> ty
      | _ -> failwith "typing error: unknown ident")
  | EApp (e1, e2) -> (
      match typeof i ctx e1 with
      | Pi (t, t') ->
          check i ctx e2 t;
          t' (interpret ctx e2)
      | _ -> failwith "typing error: type is not applicable ")
  | EStar -> Star
  | EPi (r, f) ->
      check i ctx r Star;
      let ty = interpret ctx r in
      let arg, body = opened f in
      check i (Ctx.add_name_ty ctx arg ty) body Star;
      Star
  | ESig (r, r') -> failwith "todo"
  | _ -> failwith "typing: expr is not inferrable"

and check i ctx expr ty =
  match expr with
  | EFun f -> (
      match ty with
      | Pi (t, t') ->
          let arg, body = opened f in
          check (i + 1) (Ctx.add_name_ty ctx arg t) body (t' (vfree arg))
      | _ -> failwith "typing: ill-typed expr")
  | ETuple (e1, e2) -> (
      match ty with
      | Sig (t1, t2) ->
          check i ctx e1 t1;
          check i ctx e2 (t2 (interpret ctx e1))
      | _ -> failwith "typing: ill-typed expr")
  | _ ->
      if quote (typeof i ctx expr) <> quote ty then
        failwith "typing: ill-typed expr"
