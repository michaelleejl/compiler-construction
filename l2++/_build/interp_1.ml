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

and value = 
     | INT of int 
     | BOOL of bool
     | SKIP
     | CLOSURE of closure
     | REC_CLOSURE of closure

and closure = var * expr * env 

and continuation_action = 
  | UNARY of unary_oper 
  | OPER of oper * value 
  | OPER_FST of Ast.expr * env * Ast.oper 
  | TAIL of Ast.expr list * env
  | IF of Ast.expr * Ast.expr * env
  | ASSIGN of Ast.loc
  | ARG of Ast.expr * env
  | APPLY of closure

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

let make_fun (x, body) env =
  let fvars = Free_vars.free_vars([x], body) in (*Compute ONLY the free variables in the fn body*)
  let reduced_env = filter_env fvars env in (*reduce the environment to store only the bindings for these free variables*)
  CLOSURE(x, body, reduced_env)

let make_rec_fun x (y, body) env = 
  let fvars = Free_vars.free_vars([x; y], body) in
  let reduced_env = filter_env fvars env in 
  REC_CLOSURE(x, Lambda(y, body), (x, REC_CLOSURE(x, Lambda(y, body), []))::reduced_env)
  (*Christ. Ok, first of all, we do exactly the same thing as make_fun. HOWEVER, the function body, body
    might contain recursive references to itself - for example, x = (fn y => (x (y-1)) + (x (y-2)))
    so after reducing the environment, we need to extend it with the binding for x.
    That is, we want to cons (x, value) to the reduced environment 
    Step 2: What does x bind to? What should value be? 
    x binds to exactly the thing we're trying to return in the make_rec_fun function. 
    Step 3: How do we express that? Well it should be a recursive closure, REC_CLOSURE, that keeps track
    of its variable name, x, the function it binds to, (y, body), and the environment you need to evaluate
    (y, body), which turns out to be what we're computing in step 3. 
    Step 4: Avoiding infinity - clearly, this is an infinite term. However, what we can do is say that we ignore
    we will only ever try to lookup the binding for x or some variable in reduced_env. If we look up the binding for x,
    to perform the computation, we might need a binding for x or for some variable in reduced_env. So we actually
    already have all the information we need, we just need to reconstruct it on lookup. 
  *)

let rec find env x = 
  match env with
  | [] -> complain ("variable " ^ x ^ " not instantiated")
  | (var, valu)::env -> if x = var 
                        then match valu with 
                             | REC_CLOSURE(x, Lambda(y, body), _) ->
                               CLOSURE(y, body, (x, REC_CLOSURE(x, Lambda(y, body), env))::env)
                        (*if you see x v, and you look up x and x is a recursive function*)
                        (*of the form x = fn y => body, where body contains x*) 
                        (*then i want you to return a regular function that takes in y*)
                        (*and evaluates the body, but retaining the information that x is a recursive fn*)  
                             | _ -> valu 
                        else find env x

let step = function 
 (* EXAMINE --> EXAMINE *) 
 | EXAMINE(UnaryOp(op, e),              env, k) -> EXAMINE(e,  env, (UNARY op) :: k)
 | EXAMINE(Op(e1, op, e2),              env, k) -> EXAMINE(e1, env, OPER_FST(e2, env, op) :: k)
 | EXAMINE(Seq [e],                     env, k) -> EXAMINE(e, env, k) 
 | EXAMINE(Seq (e :: rest),             env, k) -> EXAMINE(e, env, TAIL (rest, env) :: k) 
 | EXAMINE(If (e1, e2, e3),             env, k) -> EXAMINE(e1, env, IF (e2, e3, env) :: k)
 | EXAMINE(While (e1, e2),              env, k) -> EXAMINE(e1, env, IF(Seq([e2; While(e1, e2)]), Skip, env)::k)
 | EXAMINE(Assign(l, e),                env, k) -> EXAMINE(e, env, ASSIGN(l)::k)
 | EXAMINE(App(e1, e2),                 env, k) -> EXAMINE(e1, env, ARG(e2, env)::k) (*To evaluate e1 e2, first reduce e1 to a value, then [continuation] reduce e2 to a value*)
 | EXAMINE(LetRecFn(x, l, e),           env, k) -> EXAMINE(e, (x, (make_rec_fun x l env))::env, k)

 (* EXAMINE --> COMPUTE *) 
 | EXAMINE(Integer n,           _, k) -> COMPUTE(k, INT n)
 | EXAMINE(Skip,                _, k) -> COMPUTE(k, SKIP)
 | EXAMINE(Bool b,              _, k) -> COMPUTE(k, BOOL b)
 | EXAMINE(Lambda l,          env, k) -> COMPUTE(k, make_fun l env)
 | EXAMINE(Deref(l),            _, k) -> COMPUTE(k, heap.(l))
 | EXAMINE(Var(x),            env, k) -> COMPUTE(k, find env x)

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
 | COMPUTE(APPLY(x, body, env)::k,    v) -> EXAMINE(body, update (env, (x, v)), k)                                     
 | COMPUTE(ARG(e, env)::k, CLOSURE(x, body, cenv)) -> EXAMINE(e, env, APPLY(x, body, cenv)::k) (* e1 e2. Having reduced e1 to a value f, reduce e2 to a value v, then continue by applying f to v - which is at the top of the stack*)
 (*Invariant - this will always be a closure, even if it's recursive, since the recursion will be in the closure environment cenv*)
 | state -> complain ("step : malformed state = " ^ (string_of_state state) ^ "\n")

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

    

      
    
