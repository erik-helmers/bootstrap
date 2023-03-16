open Types
open Quote
open Interpret
module Ctx = Map.Make (Atom.Ord)

exception Mismatch of { expected : term; got : term }

let mismatch ty exp = Mismatch { expected = quote exp; got = quote ty }

let rec synth ctx t =
  match t with
  | Free a -> (
      try Ctx.find a ctx
      with Not_found -> failwith "synth: unknown free ")
  | Bound _ -> failwith "synth : bound terms are illegal here"
  | App (f, x) -> (
      match synth ctx f with
      | VPi (t, t') ->
          check ctx x t;
          t' (interpret x)
      | _ -> failwith "synth : term is not applicable")
  | Fst e -> (
      match synth ctx e with
      | VSigma (t, _) -> t
      | _ -> failwith "synth : term is not a tuple")
  | Snd e -> (
      match synth ctx e with
      | VSigma (_, t) -> t (interpret (Snd e))
      | _ -> failwith "synth : term is not a tuple")
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
      | _ -> failwith "synth: term is not an enum")
  | _ -> failwith "synth : term type is not synthetisable"

and check ctx t ty =
  let ensure ty exp =
    if Quote.quote_equal ty exp then () else raise (mismatch ty exp)
  in
  match t with
  | Star -> ensure VStar ty
  | Pi (t, t') ->
      ensure ty VStar;
      check ctx t VStar;
      check_binder ctx t' (interpret t) (fun _ -> VStar)
  | Sigma (t, t') ->
      ensure ty VStar;
      check ctx t VStar;
      check_binder ctx t' (interpret t) (fun _ -> VStar)
  | Lam f -> (
      match ty with
      | VPi (t, t') ->
          check_binder ctx f t (fun arg -> t' (VNeu (NVar arg)))
      | _ -> failwith "check: unexpected lambda")
  | Tuple (x, y) -> (
      match ty with
      | VSigma (t, t') ->
          check ctx x t;
          check ctx y (t' @@ interpret x)
      | _ -> failwith "check: unexpected tuple")
  | Unit -> ensure ty VStar
  | Nil -> ensure ty VUnit
  | LabelTy -> ensure ty VStar
  | Label _ -> ensure ty VLabelTy
  | LabelsTy -> ensure ty VStar
  | NilL -> ensure ty VLabelsTy
  | ConsL (l, ls) ->
      ensure ty VLabelsTy;
      check ctx l VLabelTy;
      check ctx ls VLabelsTy
  | Enum ls ->
      ensure ty VStar;
      check ctx ls VLabelsTy
  | EnumZe -> (
      match ty with
      | VEnum (VConsL _) -> ()
      | _ -> failwith "check: unexpected index")
  | EnumSuc i -> (
      match ty with
      | VEnum (VConsL (_, ls)) -> check ctx i (VEnum ls)
      | _ -> failwith "check: unexpected index")
  | _ -> ensure (synth ctx t) ty

and check_binder ctx b arg_ty out_ty =
  let arg, body = open_ b in
  check (Ctx.add arg arg_ty ctx) body (out_ty arg)
