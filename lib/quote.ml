open Types

type env = value list

let index_of env var =
  let rec aux i = function
    | [] -> raise Not_found
    | x :: xs -> if x = var then i else aux (i + 1) xs
  in
  aux 0 env

let rec quote env = function
  | VNeu n -> quote_neutral env n
  | VLam f -> Lam (quote_binder env f)
  | VPi (v, f) -> Pi (quote env v, quote_binder env f)
  | VTuple (v, v') -> Tuple (quote env v, quote env v')
  | VSigma (v, f) -> Sigma (quote env v, quote_binder env f)
  | VStar -> Star
  | VUnit -> Unit
  | VNil -> Nil
  | VLabelTy -> LabelTy
  | VLabel s -> Label s
  | VLabelsTy -> LabelsTy
  | VNilL -> NilL
  | VConsL (l, ls) -> ConsL (quote env l, quote env ls)
  | VEnum t -> Enum (quote env t)
  | VEnumZe -> EnumZe
  | VEnumSuc t -> EnumSuc (quote env t)

and quote_neutral env = function
  | NVar a -> ( try Bound (index_of env a) with Not_found -> Free a)
  | NApp (n, v) -> App (quote_neutral env n, quote env v)
  | NFst n -> Fst (quote_neutral env n)
  | NSnd n -> Snd (quote_neutral env n)
  | NRecord (l, t) -> Record (quote_neutral env l, quote_binder env t)
  | NCase (e, t, cs) ->
      Case (quote_neutral env e, quote_binder env t, quote env cs)

and quote_binder env f =
  let fresh = Atom.make (Printf.sprintf "q%d" (List.length env)) in
  let scoped = quote (fresh :: env) (f @@ VNeu (NVar fresh)) in
  close_ fresh scoped

let quote = quote []
let quote_equal v v' = Types.equal_term (quote v) (quote v')
