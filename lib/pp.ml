open PPrint
open Types

let of_pp pp t =
  string
    Format.(
      fprintf str_formatter "%a" pp t;
      flush_str_formatter ())

let atom = of_pp Atom.pp

let lam arg body =
  parens @@ flow (break 1) [ string "fun"; arg; string "->"; body ]

let rec term e =
  match e with
  | Free a -> atom a
  | Bound i -> brackets @@ OCaml.int i
  | Lam f ->
      let arg, body = open_ f in
      lam (atom arg) (term body)

let to_pp pp (fmt : Format.formatter) t =
  ToFormatter.pretty 0.8 80 fmt (pp t)

let to_show pp t = ToChannel.pretty 0.80 80 Out_channel.stdout (pp t)
let pp_atom = to_pp atom
let pp_term = to_pp term
let show_atom = to_show atom
let show_term = to_show term
