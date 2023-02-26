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

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"

let get_loc = Parsing.symbol_start_pos 

# 21 "parser.ml"
let yytransl_const = [|
  258 (* ADD *);
  259 (* SUB *);
  260 (* MUL *);
  261 (* DIV *);
  262 (* SEMICOLON *);
  263 (* LPAREN *);
  264 (* RPAREN *);
  265 (* BEGIN *);
  266 (* END *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\001\000\003\000\003\000\003\000\003\000\
\003\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\000\000\000\000\012\000\004\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\003\000\000\000\009\000\000\000\000\000\007\000\008\000\011\000"

let yydgoto = "\002\000\
\006\000\007\000\010\000\011\000"

let yysindex = "\015\000\
\008\255\000\000\000\000\008\255\008\255\000\000\000\000\019\000\
\018\255\002\255\248\254\008\255\008\255\008\255\008\255\000\000\
\000\000\008\255\000\000\020\255\020\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\017\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\010\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\255\255\011\000"

let yytablesize = 280
let yytable = "\008\000\
\005\000\019\000\009\000\012\000\013\000\014\000\015\000\018\000\
\003\000\006\000\020\000\021\000\022\000\023\000\004\000\001\000\
\005\000\000\000\016\000\012\000\013\000\014\000\015\000\014\000\
\015\000\017\000\010\000\000\000\024\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\005\000\005\000\000\000\000\000\005\000\000\000\
\005\000\000\000\005\000\006\000\006\000\000\000\000\000\006\000\
\000\000\006\000\000\000\006\000\012\000\013\000\014\000\015\000"

let yycheck = "\001\000\
\000\000\010\001\004\000\002\001\003\001\004\001\005\001\006\001\
\001\001\000\000\012\000\013\000\014\000\015\000\007\001\001\000\
\009\001\255\255\000\000\002\001\003\001\004\001\005\001\004\001\
\005\001\008\001\010\001\255\255\018\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\255\255\255\255\006\001\255\255\
\008\001\255\255\010\001\002\001\003\001\255\255\255\255\006\001\
\255\255\008\001\255\255\010\001\002\001\003\001\004\001\005\001"

let yynames_const = "\
  ADD\000\
  SUB\000\
  MUL\000\
  DIV\000\
  SEMICOLON\000\
  LPAREN\000\
  RPAREN\000\
  BEGIN\000\
  END\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Past.expr) in
    Obj.repr(
# 27 "parser.mly"
                         ( _1 )
# 167 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 30 "parser.mly"
                                     ( Past.Integer (get_loc(), _1) )
# 174 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Past.expr) in
    Obj.repr(
# 31 "parser.mly"
                                     ( _2 )
# 181 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 34 "parser.mly"
                                     (  _1 )
# 188 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 35 "parser.mly"
                                     ( Past.Op(get_loc(), _1, Past.ADD, _3) )
# 196 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 36 "parser.mly"
                                     ( Past.Op(get_loc(), _1, Past.SUB, _3) )
# 204 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 37 "parser.mly"
                                     ( Past.Op(get_loc(), _1, Past.MUL, _3) )
# 212 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 38 "parser.mly"
                                     ( Past.Op(get_loc(), _1, Past.DIV, _3) )
# 220 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Past.expr list) in
    Obj.repr(
# 39 "parser.mly"
                                     ( Past.Seq(get_loc(), _2) )
# 227 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 42 "parser.mly"
                                     ( [_1] )
# 234 "parser.ml"
               : Past.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr list) in
    Obj.repr(
# 43 "parser.mly"
                                     ( _1 :: _3  )
# 242 "parser.ml"
               : Past.expr list))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Past.expr)