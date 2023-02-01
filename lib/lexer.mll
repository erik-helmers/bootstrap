{
  open Parser

  exception Error of string

  let keywords = Hashtbl.of_seq @@ List.to_seq [
    "let", LET;
    "in" , IN;
    "fun", FUN;
    "assume", ASSUME;
  ]

}

let alpha = ['A'-'Z' 'a'-'z' ]
let num   = ['0'-'9']

let core_op_char = [ '$' '&' '*'  '/'  '+'  '-'   '='  '>' '@' '^' '|' ]
let op_char = core_op_char | [ '~' '!' '?' '%' '<' ':' ]

rule token = parse
| eof { EOF }
| [' ' '\t' '\n']
    { token lexbuf }
|  (alpha | num | '_')  ( alpha | num | '_' | '\'' ) * as id {
    try Hashtbl.find keywords id
      with Not_found -> IDENT id
    }
| "Π" | "∀" (*legacy*) { PI }
| '*' { STAR }
| '.' { DOT }
| ',' { COMMA }
| "->"
    { ARROW }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '='
    { EQ }
| ':'
    { COLON }

| [ '<' ] op_char * as op
    { INFIX_0 op }
| ['<' '='] op_char * as op
    { INFIX_1 op }
| ['+' '-'] op_char * as op
    { INFIX_2 op }
| ['*' '/'] op_char * as op
    { INFIX_3 op }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
