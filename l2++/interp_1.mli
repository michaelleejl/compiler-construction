
type address = int 

type value = 
     | INT of int 
     | BOOL of bool
     | SKIP
     | CLOSURE of closure
     | REC_CLOSURE of closure
and closure = Ast.var * Ast.expr * env 

and continuation_action = 
  | UNARY of Ast.unary_oper
  | OPER of Ast.oper * value
  | OPER_FST of Ast.expr * env * Ast.oper 
  | TAIL of Ast.expr list * env
  | IF of Ast.expr * Ast.expr * env
  | ASSIGN of Ast.loc
  | ARG of Ast.expr * env
  | APPLY of closure

and continuation = continuation_action  list

and binding = Ast.var * value

and env = binding list

type state = 
   | EXAMINE of Ast.expr * env * continuation 
   | COMPUTE of continuation * value 

val step : state -> state 

val driver : int -> state -> value 

val eval : Ast.expr * env -> value 

val interpret : Ast.expr -> value 

val string_of_value : value -> string 

