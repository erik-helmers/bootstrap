open Types
open Syntax

module Env = struct
  include Atom.Map

  type t = value Atom.Map.t
end

exception BadTerm of string * term
exception BadValue of string * value

let rec interpret env t =
  match t with
  | Free a -> (
      match Env.find_opt a env with Some v -> v | _ -> VNeu (NVar a))
  | Bound _ -> failwith "interpret: bound term are illegal here"
  | Lam f -> VLam (interpret_binder env f)
  | App (f, t) -> (
      match interpret env f with
      | VLam f -> f (interpret env t)
      | VNeu f -> VNeu (NApp (f, interpret env t))
      | _ -> failwith "interpret: value is not applicable")
  | Pi (t, f) -> VPi (interpret env t, interpret_binder env f)
  | Tuple (t, t') -> VTuple (interpret env t, interpret env t')
  | Fst t -> (
      match interpret env t with
      | VTuple (t, _) -> t
      | VNeu n -> VNeu (NFst n)
      | _ -> failwith "interpret: value is not a tuple")
  | Snd t -> (
      match interpret env t with
      | VTuple (_, t) -> t
      | VNeu n -> VNeu (NSnd n)
      | _ -> failwith "interpret: value is not a tuple")
  | Sigma (t, f) -> VSigma (interpret env t, interpret_binder env f)
  | Annot (x, _) -> interpret env x
  | Star -> VStar
  | Unit -> VUnit
  | Nil -> VNil
  | LabelTy -> VLabelTy
  | Label s -> VLabel s
  | LabelsTy -> VLabelsTy
  | NilL -> VNilL
  | ConsL (l, ls) -> (
      match (interpret env l, interpret env ls) with
      | (VLabel _ as l), (VConsL _ as ls) -> VConsL (l, ls)
      | (VLabel _ as l), (VNilL as ls) -> VConsL (l, ls)
      | _ -> failwith "interpret: invalid labels")
  | Enum ls -> (
      match interpret env ls with
      | (VConsL _ | VNilL) as ls -> VEnum ls
      | _ -> failwith "interpret: invalid labels in enum")
  | EnumZe -> VEnumZe
  | EnumSuc l -> (
      match interpret env l with
      | (VEnumZe | VEnumSuc _) as l -> VEnumSuc l
      | _ -> failwith "interpret: invalid enum index")
  | Record (l, t) ->
      let rec aux v p =
        match v with
        | VNilL -> VUnit
        | VConsL (_, e') ->
            VSigma
              ( interpret env (app (Lam p) EnumZe),
                fun _ ->
                  aux e' (binder "i" (fun i -> app (Lam p) (EnumSuc i))) )
        | VNeu n -> VNeu (NRecord (n, interpret_binder env t))
        | _ -> raise (BadValue ("case: expected some labels", v))
      in
      aux (interpret env l) t
  | Case (e, t, cs) ->
      let rec aux e cs =
        match e with
        | VEnumZe -> interpret env (Fst cs)
        | VEnumSuc e -> aux e (Snd cs)
        | VNeu n ->
            VNeu (NCase (n, interpret_binder env t, interpret env cs))
        | _ -> raise (BadValue ("switch: expected an index", e))
      in
      aux (interpret env e) cs

and interpret_binder env b x =
  let arg, body = open_ b in
  interpret (Env.add arg x env) body

let interpret = interpret Env.empty
