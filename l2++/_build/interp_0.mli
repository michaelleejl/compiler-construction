type address 

type var = string

type store = address -> value 

and value = 
     | INT of int 
     | BOOL of bool
     | SKIP
     | LAMBDA of var * Ast.expr

type env = Ast.var -> value 

val string_of_value : value -> string 

val interpret :  Ast.expr * env * store -> (value * store) 

val interpret_top_level : Ast.expr -> value 