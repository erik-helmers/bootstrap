open Syntax;;
open Context;;
open Interpreter;;
open Typing;;

let parse_expr line =
  let linebuf = Lexing.from_string line in
  Parser.main Lexer.token linebuf
;;

let base_ctx = ref Context.base_ctx


module Operators = struct
    let (!!) = parse_expr;;
    let (!$) line = interpret !base_ctx !!line ;;
    let (!:) line = typeof !base_ctx !!line;;
end;;

open Operators;;

let assume ident ty =
  base_ctx := Context.add_ident_ty !base_ctx ident ty

let define ident line =
  base_ctx := Context.add_ident_val !base_ctx ident !$line
