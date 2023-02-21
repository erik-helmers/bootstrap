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
  | VBool b -> Bool b
  | VLam f -> Lam (quote_binder env f)
  | VPi (v, f) -> Pi (quote env v, quote_binder env f)
  | VTuple (v, v') -> Tuple (quote env v, quote env v')
  | VSigma (v, f) -> Sigma (quote env v, quote_binder env f)

and quote_neutral env = function
  | NVar a -> ( try Bound (index_of env a) with Not_found -> Free a)
  | NCond (n, t, v, v') ->
      Cond
        (quote_neutral env n, quote_binder env t, quote env v, quote env v')
  | NApp (n, v) -> App (quote_neutral env n, quote env v)
  | NFst n -> Fst (quote_neutral env n)
  | NSnd n -> Snd (quote_neutral env n)

and quote_binder env f =
  let fresh = Atom.make (Printf.sprintf "q%d" (List.length env)) in
  let scoped = quote (fresh :: env) (f @@ VNeu (NVar fresh)) in
  close_ fresh scoped

let quote = quote []
