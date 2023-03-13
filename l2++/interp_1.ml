(**************************************
Compiler Construction 2020
Computer Laboratory 
University of Cambridge 
Timothy G. Griffin (tgg22@cam.ac.uk) 
*****************************************) 
(* Interpreter 1. 

   Derived from Interpreter 1 via 
   CPS and DFC transformations applied
   to the code of Interp_0.interpret. 

*) 


open Ast 

let complain = Errors.complain

type address = int 

type value = 
     | INT of int 
     | BOOL of bool
     | SKIP
     | CLOSURE of closure
and closure = var * expr * env 

and continuation_action = 
  | UNARY of unary_oper 
  | OPER of oper * value 
  | OPER_FST of Ast.expr * env * Ast.oper 
  | TAIL of Ast.expr list * env
  | IF of Ast.expr * Ast.expr * env
  | ASSIGN of Ast.loc
and continuation = continuation_action  list

and binding = var * value

and env = binding list

(*EXAMINE generates a computation, COMPUTE computes the computation*)
(*The way I see this is this is the end-product of CPS, defunctionalisation, and eliminating mutual recursion*)
(*When you perform CPS, your interpreter goes from taking a list of arguments to a list of arguments and a continuation*)
(*When you do defunctionalisation, the type of the continuation changes from a lambda to a custom datatype, continuation_action, that you evaluate using an apply_cnt fn*)
(*This apply_cnt function will be mutually recursive with the interpreter, so to eliminate mutual recursion, you can define a state or tag datatype that disambiguates
  between the action of the interpreter (which EXAMINES an expression, env, and continuation, and generates an appropriate continuation and the action of apply_cnt, which
  applies a continuation to a value. 
*)
type state = 
   | EXAMINE of expr * env * continuation 
   | COMPUTE of continuation * value 


(* update : (env * binding) -> env *) 
let update(env, (x, v)) = (x, v) :: env 

(* When making a closure, only include bindings that 
   are needed. 
*) 

let rec inlist x = function 
  | [] -> false 
  | y :: rest -> (x = y) || (inlist x rest) 

let rec filter_env fvars = function 
  | [] -> [] 
  | (x, v) :: rest -> if inlist x fvars then (x, v) :: (filter_env fvars rest) else (filter_env fvars rest)


let readint () = let _ = print_string "input> " in read_int() 

let do_unary = function 
  | (NEG,  INT m)  -> INT (-m)
  | (op, _) -> complain ("malformed unary operator: " ^ (string_of_unary_oper op))

let do_oper = function 
  | (ADD,  INT m,   INT n)  -> INT (m + n)
  | (SUB,  INT m,   INT n)  -> INT (m - n)
  | (MUL,  INT m,   INT n)  -> INT (m * n)
  | (DIV,  INT m,   INT n)  -> INT (m / n)
  | (GTEQ, INT m,   INT n)  -> BOOL (m >= n)
  | (op, _, _)  -> complain ("malformed binary operator: " ^ (string_of_oper op))


let string_of_list sep f l = 
   let rec aux f = function 
     | [] -> ""
     | [t] -> (f t)
     | t :: rest -> (f t) ^  sep  ^ (aux f rest)
   in "[" ^ (aux f l) ^ "]"


let rec string_of_value = function 
     | INT n          -> string_of_int n 
     | BOOL b         -> string_of_bool b
     | SKIP           -> "skip"

and string_of_closure (x, e, env) = x ^ ", " ^ (Ast.string_of_expr e) ^  ", " ^ (string_of_env env)

and string_of_env env = string_of_list ",\n " string_of_binding env 

and string_of_binding (x, v) =    "(" ^ x ^ ", " ^ (string_of_value v) ^ ")"

let string_of_expr_list = string_of_list "; " Ast.string_of_expr

let string_of_continuation_action = function 
  | UNARY op     -> "UNARY " ^ (string_of_unary_oper op)
  | OPER (op, v) -> "OPER(" ^ (string_of_oper op) ^ ", " ^ (string_of_value v) ^ ")"
  | OPER_FST(e, env, op) ->  
      "OPER_FST(" ^ (Ast.string_of_expr e) ^ ", " ^ (string_of_env env) ^ ", " ^ (string_of_oper op) ^ ")"
  | TAIL (el , env) -> "TAIL("  ^ (string_of_expr_list el)   ^ ", " ^ (string_of_env env) ^ ")"
  | IF (e1, e2, env) -> "IF (" ^ (Ast.string_of_expr e1) ^ "," ^ (Ast.string_of_expr e2) ^ ", " ^ (string_of_env env)
  | ASSIGN(l)       -> "ASSIGN" ^ string_of_int l
let string_of_continuation = string_of_list ";\n " string_of_continuation_action

let string_of_state = function 
   | EXAMINE(e, env, cnt) -> 
      "EXAMINE(" ^ (Ast.string_of_expr e) ^ ", " 
              ^ (string_of_env env) ^ ", " 
              ^ (string_of_continuation cnt) ^ ")" 
   | COMPUTE(cnt, v)     -> 
      "COMPUTE(" ^ (string_of_continuation cnt) ^ ", " 
               ^ (string_of_value v) ^ ")"


let heap  = Array.make Option.heap_max (INT 0)

let next_address = ref 0 

let new_address () = let a = !next_address in (next_address := a + 1; a) 

let do_assign a v = (heap.(a) <- v)

let step = function 
 (* EXAMINE --> EXAMINE *) 
 | EXAMINE(UnaryOp(op, e),              env, k) -> EXAMINE(e,  env, (UNARY op) :: k)
 | EXAMINE(Op(e1, op, e2),              env, k) -> EXAMINE(e1, env, OPER_FST(e2, env, op) :: k)
 | EXAMINE(Seq [e],                     env, k) -> EXAMINE(e, env, k) 
 | EXAMINE(Seq (e :: rest),             env, k) -> EXAMINE(e, env, TAIL (rest, env) :: k) 
 | EXAMINE(If (e1, e2, e3),             env, k) -> EXAMINE(e1, env, IF (e2, e3, env) :: k)
 | EXAMINE(While (e1, e2),              env, k) -> EXAMINE(e1, env, IF(Seq([e2; While(e1, e2)]), Skip, env)::k)
 | EXAMINE(Assign(l, e),                env, k) -> EXAMINE(e, env, ASSIGN(l)::k)
 (* EXAMINE --> COMPUTE *) 
 | EXAMINE(Integer n,         _, k) -> COMPUTE(k, INT n)
 | EXAMINE(Skip,              _, k) -> COMPUTE(k, SKIP)
 | EXAMINE(Bool b,            _, k) -> COMPUTE(k, BOOL b)
 | EXAMINE(Deref(l),          _, k) -> COMPUTE(k, heap.(l))
 (* COMPUTE --> COMPUTE *) 
 | COMPUTE((UNARY op) :: k,    v) -> COMPUTE(k, (do_unary(op, v)))
 | COMPUTE(OPER(op, v1) :: k, v2) -> COMPUTE(k, do_oper(op, v1, v2))
 | COMPUTE(ASSIGN(l):: k,      v) -> let _ = do_assign l v in COMPUTE(k, SKIP)
 (* COMPUTE --> EXAMINE *) 
 | COMPUTE(OPER_FST (e2, env, op) :: k,         v1)  -> EXAMINE(e2, env, OPER (op, v1) :: k)
 | COMPUTE((TAIL (el, env)) :: k,     _)  ->  EXAMINE(Seq el, env, k) 
 | COMPUTE(IF(e2, e3, env)::k, v) -> (match v with 
                                      | BOOL(true) -> EXAMINE(e2, env, k)
                                      | BOOL(false) -> EXAMINE(e3, env, k)
                                      | state -> complain ("step : malformed state"))
 | state -> complain ("step : malformed state = " ^ (string_of_state state) ^ "\n")


 (*
\       | App of         expr * expr
       | Lambda of      lambda
       | LetRecFn of    var * lambda * expr
       | Var of var   
 *)
 (*Driver drives the state*)
let rec driver n state = 
  let _ = if Option.verbose 
          then print_string ("\nstate " ^ (string_of_int n) ^ " = \n" ^ (string_of_state state) ^ "\n")
          else () 
  in match state with 
     | COMPUTE([], v) -> v 
     | _              -> driver (n + 1) (step state) 

(*Top level function - called when the interpreter is run*)     
let eval(e, env) = driver 1 (EXAMINE(e, env, []))

(* env_empty : env *) 
let env_empty = []

(* interpret : expr -> value *) 
let interpret e = eval(e, env_empty)

    

      
    
