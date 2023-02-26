type token =
  | INT of (int)
  | IDENT of (String)
  | LOCAT of (String)
  | SKIP
  | TRUE
  | FALSE
  | ADD
  | SUB
  | MUL
  | DIV
  | EQUALS
  | GTEQ
  | ASSIGN
  | LPAREN
  | RPAREN
  | SEMICOLON
  | COLON
  | ARROW
  | BANG
  | DARROW
  | END
  | WHILE
  | DO
  | IF
  | THEN
  | ELSE
  | LAMBDA
  | LET
  | IN
  | LETREC
  | FUN
  | BEGIN
  | INTTYPE
  | BOOLTYPE
  | UNITTYPE
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Past.expr
