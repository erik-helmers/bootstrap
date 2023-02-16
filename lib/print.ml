open Types
open PPrint

let pi = fancystring "Π" 1
let si = fancystring "Σ" 1

let is_atomic expr =
  match expr with EFree _ | EBound _ | EStar -> true | _ -> false

let doc_of_atom atom =
  !^(Printf.sprintf "%s@%d" (Atom.name atom) (Atom.uid atom))

let rec protect i expr =
  if is_atomic expr then doc_of_expr i expr
  else parens (doc_of_expr i expr)

and doc_of_expr =
  let rec aux i (expr : expr) =
    match expr with
    | EFree atom -> doc_of_atom atom
    | EBound i -> brackets @@ !^(string_of_int i)
    | EStar -> !^"*"
    | ETuple (a, b) -> parens @@ aux i a ^^ !^"," ^/^ aux i b
    | EAnnot (v, t) -> parens @@ aux i v ^^ !^":" ^/^ aux i t
    | EApp (f, x) -> protect i f ^//^ protect i x
    | EFun f ->
        let arg, body = opened f in
        !^"fun" ^/^ doc_of_atom arg ^/^ !^"->" ^//^ aux (i + 1) body
    | EPi (r, f) ->
        let arg, body = opened f in
        pi ^/^ doc_of_atom arg ^/^ !^":"
        ^/^ protect (i + 1) r
        ^/^ !^"."
        ^//^ aux (i + 1) body
    | ESig (a, b) -> failwith "todo"
  in
  aux

let pp_expr fmt expr =
  ToFormatter.pretty 0.80 80 fmt (doc_of_expr 0 expr |> braces)
