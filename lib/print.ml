open Syntax;;
open PPrint;;

let pi = fancystring "Π" 1
let si = fancystring "Σ" 1

let is_atom expr = match expr with
  | EFree _ | EBound _ | EStar -> true
  | _ -> false


let doc_of_name i name  = match name with
  | Global id -> !^id
  | Quote j
  | Local j -> char @@ char_of_int (j-i+97)


let rec protect i expr =
  if is_atom expr then doc_of_expr i expr
  else parens (doc_of_expr i expr)
and doc_of_expr = let rec aux i (expr:expr) = match expr with
| EFree name -> doc_of_name i name
| EBound i -> doc_of_name i (Local i)
| EStar -> !^"*"
| ETuple (a, b) -> parens @@ (aux i a ^^ !^"," ^/^ aux i b)
| EAnnot (v, t) -> parens @@ (aux i v ^^ !^":" ^/^ aux i t)
| EApp (f, x) -> protect i f ^//^ protect i x
| EFun f -> !^"fun" ^/^ doc_of_name i (Local 0) ^/^ !^"->" ^//^ aux (i+1) f
| EPi (a, b) -> pi ^/^ doc_of_name i (Local 0) ^/^ !^":" ^/^ protect (i+1) a ^/^ !^"." ^//^ aux (i+1) b
| ESig (a, b) -> si ^/^ doc_of_name i (Local 0) ^/^ !^":" ^/^ protect (i+1) a ^/^ !^"." ^//^ aux (i+1) b
in aux;;


let pretty = ToFormatter.pretty 0.80 80 Format.std_formatter;;
let print_expr expr = ToChannel.pretty 0.80 80 Out_channel.stdout (doc_of_expr 0 expr)
