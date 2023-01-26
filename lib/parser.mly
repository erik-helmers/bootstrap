

%{ open Syntax %}

%start <Syntax.expr> main
%type  <Syntax.expr> expr

%token <string> IDENT

%token <string> INFIX_0 ">"
%token <string> INFIX_1 "<"
%token <string> INFIX_2 "+"
%token <string> INFIX_3 "*"

%token LET    "let"
%token IN     "in"
%token FUN    "fun"
%token ARROW  "->"
%token COLON  ":"

%token LPAREN "("
%token RPAREN ")"
%token EQ     "="

%token EOF

%left ">"
%left "<"
%left "+"
%left "*"

%%

let main :=
  ~ = expr; EOF; <>

let ident ==
    | ~ = IDENT; <>
    | "("; ~ = any_op; ")"; <>


let any_op ==
  | "="; {"="}
  | ~ = INFIX_0; <>
  | ~ = INFIX_1; <>
  | ~ = INFIX_2; <>
  | ~ = INFIX_3; <>

let ty :=
  | ~ = ty_ato; <>
  | a = ty_ato; "->"; b = ty; <TFun>

let ty_ato ==
  | ~ = ident; <Ty>
  | "("; ~ = ty; ")"; <>


let expr ==
  fun_expr

let fun_expr :=
  | bind_expr
  | "fun"; args = list(ident); "->"; body=expr;
        { fold_args args body }

let bind_expr :=
  | binop_expr
  |  "let";  (id,body) = bind_op;
     "in";  ~ = expr; { EApp(EFun(id,expr), body)  }

let bind_op ==
  | ident = ident; args = list(ident);
      "="; body = expr; {ident, fold_args args body}

let fold_binop(op, elem) :=
  | elem
  | sum = fold_binop(op, elem); ~ = op; ~ = elem;
        { EApp(EApp(op, sum), elem) }

let binop_expr :=
  | application_expr
  | left = binop_expr; ~ = binop_op; right = binop_expr;
        { EApp(EApp(binop_op, left), right) }

let binop_op ==
  | ~ = INFIX_0; <EIdent>
  | ~ = INFIX_1; <EIdent>
  | ~ = INFIX_2; <EIdent>
  | ~ = INFIX_3; <EIdent>

let application_expr :=
  | atomic_expr
  | ~ = application_expr; ~ = atomic_expr; <EApp>

let atomic_expr :=
  | "("; ~ = expr;  ")"; <>
  | "("; ~ = expr; ":"; ~ = ty; ")"; < EAnnot >
  | ~ = ident; <EIdent>
