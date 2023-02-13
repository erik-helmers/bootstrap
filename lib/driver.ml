open Types
open Interpreter
open Typing
include Print

let parse f line =
  let linebuf = Lexing.from_string line in
  f Lexer.token linebuf

let parse_top = parse Parser.main
let parse_expr = parse Parser.standalone_expr
let parse_ty = parse Parser.standalone_ty
let ctx = ref Context.base_ctx
let assume ident ty = ctx := Context.add_name_ty !ctx (Global ident) ty
let define ident v = ctx := Context.add_name_val !ctx (Global ident) v

let top line =
  match parse_top line with
  | TAssign (id, expr) ->
      ctx := Context.add_name_val !ctx (Global id) (interpret !ctx expr);
      ctx := Context.add_name_ty !ctx (Global id) (typeof 0 !ctx expr)
  | TAssume (id, expr) ->
      ctx := Context.add_name_val !ctx (Global id) (vvar id);
      ctx := Context.add_name_ty !ctx (Global id) (interpret !ctx expr)

module Operators = struct
  let ( !! ) = parse_expr
  let ( !$ ) line = interpret !ctx !!line
  let ( !: ) line = typeof 0 !ctx !!line
end
