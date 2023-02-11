open Syntax
open Interpreter
module Ctx = Context

let subst v =
  let rec aux i expr =
    match expr with
    | EApp (f, x) -> EApp (aux i f, aux i x)
    | EFun f -> EFun (aux (i + 1) f)
    | EPi (a, b) -> EPi (aux i a, aux (i + 1) b)
    | ESig (a, b) -> ESig (aux i a, aux (i + 1) b)
    | ETuple (a, b) -> ETuple (aux i a, aux i b)
    | EAnnot (a, b) -> EAnnot (aux i a, aux i b)
    | EBound j -> if i = j then v else expr
    | EStar | EFree _ -> expr
  in
  aux 0

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
  | EPi (r, r') ->
      check i ctx r Star;
      let ty = interpret ctx r in
      check i (Ctx.add_name_ty ctx (Local i) ty) r' Star;
      Star
  | ESig (r, r') ->
      check i ctx r Star;
      let ty = interpret ctx r in
      check i (Ctx.add_name_ty ctx (Local i) ty) r' Star;
      Star
  | _ -> failwith "typing: expr is not inferrable"

and check i ctx expr ty =
  match expr with
  | EFun body -> (
      match ty with
      | Pi (t, t') ->
          check (i + 1)
            (Ctx.add_name_ty ctx (Local i) t)
            (subst (EFree (Local i)) body)
            (t' (vfree (Local i)))
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
