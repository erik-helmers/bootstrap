open Types
open Syntax
open Quote

module Env = struct
  include Atom.Map

  type t = value Atom.Map.t
end

let rec interpret env t =
  match t with
  | Free a -> (
      match Env.find_opt a env with Some v -> v | _ -> VNeu (NVar a))
  | Bound _ -> raise (bad_term "interpret: bound term are illegal here" t)
  | Lam f -> VLam (interpret_binder env f)
  | App (f, t) -> (
      match interpret env f with
      | VLam f -> f (interpret env t)
      | VNeu f -> VNeu (NApp (f, interpret env t))
      | _ -> raise (bad_term "interpret: value is not applicable" t))
  | Pi (t, f) -> VPi (interpret env t, interpret_binder env f)
  | Tuple (t, t') -> VTuple (interpret env t, interpret env t')
  | Fst t -> (
      match interpret env t with
      | VTuple (t, _) -> t
      | VNeu n -> VNeu (NFst n)
      | _ -> raise (bad_term "interpret: value is not a tuple" t))
  | Snd t -> (
      match interpret env t with
      | VTuple (_, t) -> t
      | VNeu n -> VNeu (NSnd n)
      | _ -> raise (bad_term "interpret: value is not a tuple" t))
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
      | (VLabel _ as l), ((VConsL _ | VNilL) as ls) -> VConsL (l, ls)
      | _ -> raise (bad_term "interpret: bad labels" t))
  | Enum ls -> (
      match interpret env ls with
      | (VConsL _ | VNilL) as ls -> VEnum ls
      | _ -> raise (bad_term "interpret: bad labels" ls))
  | EnumZe -> VEnumZe
  | EnumSuc l -> (
      match interpret env l with
      | (VEnumZe | VEnumSuc _) as l -> VEnumSuc l
      | _ -> raise (bad_term "interpret: invalid enum index" l))
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
        | _ -> raise (bad_value "record: expected some labels" v)
      in
      aux (interpret env l) t
  | Case (e, t, cs) ->
      let rec aux e cs =
        match e with
        | VEnumZe -> interpret env (Fst cs)
        | VEnumSuc e -> aux e (Snd cs)
        | VNeu n ->
            VNeu (NCase (n, interpret_binder env t, interpret env cs))
        | _ -> raise (bad_value "case: expected an index" e)
      in
      aux (interpret env e) cs
  | DUnit -> VDUnit
  | DVar -> VDVar
  | DPi (t, t') -> VDPi (interpret env t, interpret env t')
  | DSigma (t, t') -> VDSigma (interpret env t, interpret env t')
  | Decode (d, t) ->
      let rec aux d t =
        match d with
        | VDUnit -> VUnit
        | VDVar -> interpret env t
        | VDSigma (t', d') ->
            interpret env
              (sigma "e" (quote t') (fun e -> decode (app (quote d') e) t))
        | VDPi (t', d') ->
            interpret env
              (pi "e" (quote t') (fun e -> decode (app (quote d') e) t))
        | VNeu n -> VNeu (NDecode (n, interpret env t))
        | _ -> raise (bad_value "decode: expected a desc" d)
      in
      aux (interpret env d) t
  | DescTy -> VDescTy
  | Fix t -> VFix (interpret env t)
  | In t -> VIn (interpret env t)
  | Out t -> (
      match interpret env t with
      | VIn v -> v
      | VNeu n -> VNeu (NOut n)
      | _ -> raise (bad_term "out: expected an in" t))
  | Uncurry t ->
      interpret env (fn "p" (fun p -> app2 t (first p) (second p)))

and interpret_binder env b x =
  let arg, body = open_ b in
  interpret (Env.add arg x env) body

let interpret = interpret Env.empty
