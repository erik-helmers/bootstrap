open PPrint
open Types

let pi_sym = fancystring "Π" 1
let sigma_sym = fancystring "Σ" 1

let of_pp pp t =
  string
    Format.(
      fprintf str_formatter "%a" pp t;
      flush_str_formatter ())

let atom = of_pp Atom.pp

let cond cnd x t b b' =
  parens
  @@ flow (break 1) [ string "cond"; cnd; brackets (x ^/^ t); b; b' ]

let lam arg body =
  parens @@ flow (break 1) [ string "fn"; arg; string "->"; body ]

let pi x t t' =
  flow (break 0) [ pi_sym; parens (x ^/^ colon ^/^ t); dot; t' ]

let sigma x t t' =
  flow (break 0) [ sigma_sym; parens (x ^/^ colon ^/^ t); dot; t' ]

let app t t' = parens (t ^/^ t')
let tuple t t' = OCaml.tuple [ t; t' ]
let fst t = app (string "fst") t
let snd t = app (string "snd") t
let annot x t = parens (x ^/^ colon ^/^ t)

let rec term e =
  match e with
  | Free a -> atom a
  | Bound i -> brackets @@ OCaml.int i
  | Bool true -> string "true"
  | Bool false -> string "false"
  | Cond (cnd, t, b, b') ->
      let arg, body = open_ t in
      cond (term cnd) (atom arg) (term body) (term b) (term b')
  | BoolTy -> string "bool_ty"
  | Lam f ->
      let arg, body = open_ f in
      lam (atom arg) (term body)
  | App (t, t') -> app (term t) (term t')
  | Pi (t, t') ->
      let arg, body = open_ t' in
      pi (atom arg) (term t) (term body)
  | Tuple (t, t') -> tuple (term t) (term t')
  | Fst t -> fst (term t)
  | Snd t -> snd (term t)
  | Sigma (t, t') ->
      let arg, body = open_ t' in
      sigma (atom arg) (term t) (term body)
  | Annot (x, t) -> annot (term x) (term t)
  | Star -> star
  | Unit -> string "unit"
  | Nil -> string "nil"

let to_pp pp (fmt : Format.formatter) t =
  ToFormatter.pretty 0.8 80 fmt (pp t)

let to_show pp t = ToChannel.pretty 0.80 80 Out_channel.stdout (pp t)
let pp_atom = to_pp atom
let pp_term = to_pp term
let show_atom = to_show atom
let show_term = to_show term
