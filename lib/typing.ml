open Types
open Quote
open Interpret
open Syntax
module Ctx = Map.Make (Atom.Ord)

exception Mismatch of { expected : term; got : term }

let mismatch ty exp = Mismatch { expected = quote exp; got = quote ty }

let rec synth ctx t =
  match t with
  | Free a -> (
      try Ctx.find a ctx
      with Not_found -> raise (bad_term "synth: unknown free " t))
  | Bound _ -> raise (bad_term "synth : bound terms are illegal here" t)
  | App (f, x) -> (
      match synth ctx f with
      | VPi (t, t') ->
          check ctx x t;
          t' (interpret x)
      | _ -> raise (bad_term "synth : term is not applicable" f))
  | Fst e -> (
      match synth ctx e with
      | VSigma (t, _) -> t
      | _ -> raise (bad_term "synth : term is not a tuple" e))
  | Snd e -> (
      match synth ctx e with
      | VSigma (_, t) -> t (interpret (Snd e))
      | _ -> raise (bad_term "synth : term is not a tuple" e))
  | Annot (x, t) ->
      check ctx t VStar;
      let ty = interpret t in
      check ctx x ty;
      ty
  | Record (l, t) ->
      check ctx l VLabelsTy;
      check_binder ctx t (VEnum (interpret l)) (fun _ -> VStar);
      VStar
  | Case (e, t, cs) -> (
      match synth ctx e with
      | VEnum l ->
          check_binder ctx t (VEnum l) (fun _ -> VStar);
          check ctx cs (interpret (Record (quote l, t)));
          interpret (App (Lam t, e))
      | _ -> raise (bad_term "synth: term is not an enum" e))
  | Decode (d, t) ->
      check ctx d VDescTy;
      check ctx t VStar;
      VStar
  | _ -> raise (bad_term "synth : term type is not synthetisable" t)

and check ctx t ty =
  let ensure ty exp =
    if Quote.quote_equal ty exp then () else raise (mismatch ty exp)
  in
  match t with
  | Star -> ensure VStar ty
  | Pi (t, t') ->
      ensure VStar ty;
      check ctx t VStar;
      check_binder ctx t' (interpret t) (fun _ -> VStar)
  | Sigma (t, t') ->
      ensure VStar ty;
      check ctx t VStar;
      check_binder ctx t' (interpret t) (fun _ -> VStar)
  | Lam f -> (
      match ty with
      | VPi (t, t') ->
          check_binder ctx f t (fun arg -> t' (VNeu (NVar arg)))
      | _ -> raise (bad_value "check: unexpected lambda" ty))
  | Tuple (x, y) -> (
      match ty with
      | VSigma (t, t') ->
          check ctx x t;
          check ctx y (t' @@ interpret x)
      | _ -> raise (bad_value "check: unexpected tuple" ty))
  | Unit -> ensure VStar ty
  | Nil -> ensure VUnit ty
  | LabelTy -> ensure VStar ty
  | Label _ -> ensure VLabelTy ty
  | LabelsTy -> ensure VStar ty
  | NilL -> ensure VLabelsTy ty
  | ConsL (l, ls) ->
      ensure VLabelsTy ty;
      check ctx l VLabelTy;
      check ctx ls VLabelsTy
  | Enum ls ->
      ensure VStar ty;
      check ctx ls VLabelsTy
  | EnumZe -> (
      match ty with
      | VEnum (VConsL _) -> ()
      | _ -> raise (mismatch VEnumZe ty))
  | EnumSuc i -> (
      match ty with
      | VEnum (VConsL (_, ls)) -> check ctx i (VEnum ls)
      | _ -> raise (bad_value "check: unexpected index" ty))
  | DUnit -> ensure VDescTy ty
  | DVar -> ensure VDescTy ty
  | DSigma (s, d) | DPi (s, d) ->
      ensure VDescTy ty;
      check ctx s VStar;
      check ctx d (interpret @@ pi "s" s (fun _ -> desc_ty))
  | DescTy -> ensure VStar ty
  | _ -> ensure (synth ctx t) ty

and check_binder ctx b arg_ty out_ty =
  let arg, body = open_ b in
  check (Ctx.add arg arg_ty ctx) body (out_ty arg)
