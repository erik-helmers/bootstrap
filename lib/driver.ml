open Syntax;;
open Interpreter;;

let parse_expr line =
  let linebuf = Lexing.from_string line in
  Parser.main Lexer.token linebuf
;;

module Operators = struct
    let (!) = parse_expr;;
    let (!$) line = interpret base_ctx !line ;;
end;;
