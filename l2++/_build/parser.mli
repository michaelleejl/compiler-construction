type token =
  | INT of (int)
  | ADD
  | SUB
  | MUL
  | DIV
  | SEMICOLON
  | LPAREN
  | RPAREN
  | BEGIN
  | END
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Past.expr
