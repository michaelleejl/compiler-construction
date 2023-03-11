type token =
  | INT of (int)
  | IDENT of (string)
  | LOCAT of (int)
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
  | REF
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
  | REFTYPE
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Past.expr
