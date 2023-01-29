open Syntax;;
open Context;;
open Interpreter;;
open Typing;;

let parse_expr line =
  let linebuf = Lexing.from_string line in
  Parser.main Lexer.token linebuf
;;

module Operators = struct
    let (!) = parse_expr;;
    let (!$) line = interpret base_ctx !line ;;
    let (!:) line = typeof base_ctx !line;;
end;;
