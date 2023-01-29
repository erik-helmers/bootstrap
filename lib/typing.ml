open Syntax;;

open Context;;
module Ctx = Context;;


let rec kindof ctx ty = match ty with
  | Ty id -> (match Ctx.ident_ty_opt ctx id with
      | Some HasKind Star -> ()
      | _ -> failwith "not a type")
  | TFun (ty,ty') -> kindof ctx ty; kindof ctx ty'
;;

let rec typeof ctx expr_inf = match expr_inf with
  | EAnnot (expr_sup, ty) ->
      kindof ctx ty;
      check ctx expr_sup ty;
      ty
  | EIdent id -> (match Ctx.ident_ty ctx id with
      | HasType ty -> ty
      | _ -> failwith "typing error: unknown ident")
  | EApp (e1,e2) -> (match typeof ctx e1 with
      | TFun(t,t') -> check ctx e2 t; t'
      | _ -> failwith "typing error: type is not applicable ")
  | _ -> failwith "typing: cannot infer type"
and check ctx expr ty = match expr with
  | EFun(id,body) -> (match ty with
      | TFun(t,t') -> check (Ctx.add_ident_ty ctx id (HasType t)) body t'
      | _ -> failwith "typing: ill-typed expr")
  | _ -> if typeof ctx expr <> ty then failwith "typing: ill-typed expr"
;;



