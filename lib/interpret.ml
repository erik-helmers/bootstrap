open Types

module Env = struct
  include Atom.Map

  type t = value Atom.Map.t
end

let rec interpret env t =
  match t with
  | Free a -> (
      match Env.find_opt a env with Some v -> v | _ -> VNeu (NVar a))
  | Bound _ -> failwith "interpret: bound term are illegal here"
  | Bool t -> VBool t
  | Cond (c, t, b, b') -> (
      match interpret env c with
      | VBool true -> interpret env b
      | VBool false -> interpret env b'
      | VNeu c ->
          VNeu
            (NCond
               ( c,
                 interpret_binder env t,
                 interpret env b,
                 interpret env b' ))
      | _ -> failwith "interpret: condition is not a bool")
  | BoolTy -> VBoolTy
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

and interpret_binder env b x =
  let (arg : atom), body = open_ b in
  interpret (Env.add arg x env) body

let interpret = interpret Env.empty
