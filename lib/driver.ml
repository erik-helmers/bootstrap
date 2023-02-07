open Syntax
open Interpreter
open Typing

let parse f line =
  let linebuf = Lexing.from_string line in
  f Lexer.token linebuf

let parse_top = parse Parser.main
let parse_expr = parse Parser.standalone_expr
let parse_ty = parse Parser.standalone_ty
let ctx = ref Context.base_ctx
let assume ident ty = ctx := Context.add_ident_ty !ctx ident ty
let define ident v = ctx := Context.add_ident_val !ctx ident v

let top line =
  match parse_top line with
  | TAssign (id, expr) ->
      ctx := Context.add_ident_val !ctx id (interpret !ctx expr);
      ctx := Context.add_ident_ty !ctx id (typeof !ctx expr)
  | TAssume (id, expr) ->
      ctx := Context.add_ident_val !ctx id (vvar id);
      ctx := Context.add_ident_ty !ctx id (interpret !ctx expr)

module Operators = struct
  let ( !! ) = parse_expr
  let ( !$ ) line = interpret !ctx !!line
  let ( !: ) line = typeof !ctx !!line
end
