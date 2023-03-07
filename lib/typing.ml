open Types
open Syntax
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
  | Bool _ -> VBoolTy
  | Cond (c, t, _, _) -> interpret (App (Lam t, c))
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
      check ctx x (interpret t);
      interpret t
  | _ -> failwith "synth : term type is not synthetisable"

and check ctx t ty =
  let ensure ty exp =
    if Quote.quote_equal ty exp then () else raise (mismatch ty exp)
  in
  match t with
  | Star -> ensure ty VStar
  | BoolTy -> ensure ty VStar
  | Pi (t, t') ->
      check ctx t VStar;
      check_binder ctx t' (interpret t) (fun _ -> VStar);
      ensure ty VStar
  | Sigma (t, t') ->
      check ctx t VStar;
      check_binder ctx t' (interpret t) (fun _ -> VStar);
      ensure ty VStar
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
  | _ -> ensure (synth ctx t) ty

and check_binder ctx b arg_ty out_ty =
  let arg, body = open_ b in
  check (Ctx.add arg arg_ty ctx) body (out_ty arg)
