(* 

   The Parsed AST 

*) 
type var = string 
type store_location = int

type loc = Lexing.position 

type type_expr = 
   | TEInt
   | TEBool 
   | TEUnit 
   | TEArrow of type_expr * type_expr
   | TERef of type_expr

type formals = (var * type_expr) list

type oper = ADD | MUL | DIV | SUB | GTEQ

type unary_oper = NEG 

type expr = 
       | Integer of     loc * int
       | Skip of        loc
       | Bool of        loc * bool
       | Deref of       loc * store_location
       | App of         loc * expr * expr
       | UnaryOp of     loc * unary_oper * expr
       | Op of          loc * expr * oper * expr
       | Assign of      loc * store_location * expr
       | If of          loc * expr * expr * expr
       | While of       loc * expr * expr
       | Seq of         loc * (expr list)
       | Lambda of      loc * lambda
       | Let of         loc * var * type_expr * expr * expr
       | LetRecFn of    loc * var * type_expr * lambda * expr
       | Var of         loc * var
and lambda = var * type_expr * expr 

let  loc_of_expr = function 
    | Integer (loc, _)                 -> loc 
    | Skip(loc)                        -> loc
    | Bool(loc, _)                     -> loc
    | Deref(loc, _)                    -> loc
    | App(loc, _, _)                   -> loc
    | UnaryOp(loc, _, _)               -> loc 
    | Op(loc, _, _, _)                 -> loc 
    | Assign(loc, _, _)                -> loc
    | If(loc, _, _, _)                 -> loc
    | While(loc, _, _)                 -> loc
    | Seq(loc, _)                      -> loc
	  | Lambda(loc, _)                   -> loc
    | Let(loc, _, _, _, _)             -> loc
    | LetRecFn(loc, _, _, _, _)        -> loc
    | Var(loc, _)                      -> loc


let string_of_loc loc = 
    "line " ^ (string_of_int (loc.Lexing.pos_lnum)) ^ ", " ^ 
    "position " ^ (string_of_int ((loc.Lexing.pos_cnum - loc.Lexing.pos_bol) + 1))

open Format

(*
   Documentation of Format can be found here: 
   http://caml.inria.fr/resources/doc/guides/format.en.html
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
*) 

let rec pp_type = function 
  | TEInt -> "int" 
  | TEBool -> "bool" 
  | TEUnit -> "unit" 
  | TEArrow(t1, t2)   -> "(" ^ (pp_type t1) ^ " -> " ^ (pp_type t2) ^ ")" 
  | TERef t -> (pp_type t) ^ "ref"

let pp_uop = function 
  | NEG -> "-" 

let pp_bop = function 
  | ADD  -> "+" 
  | MUL  -> "*" 
  | DIV  -> "/" 
  | SUB  -> "-" 
  | GTEQ -> ">="

let string_of_oper = pp_bop 
let string_of_unary_oper = pp_uop 

let fstring ppf s = fprintf ppf "%s" s
let pp_type ppf t = fstring ppf (pp_type t) 
let pp_unary ppf op = fstring ppf (pp_uop op) 
let pp_binary ppf op = fstring ppf (pp_bop op) 

(* ignore locations *) 
let rec pp_expr ppf = function 
    | Integer (_, n)                      -> fstring ppf (string_of_int n)
    | Skip (_)                            -> fstring ppf "skip"
    | Bool(_, b)                          -> fstring ppf (string_of_bool b)
    | Deref(_, s)                         -> fprintf ppf "!%i" s
    | App(_, e1, e2)                      -> fprintf ppf "(%a)(%a)" pp_expr e1 pp_expr e2
    | UnaryOp(_, op, e)                   -> fprintf ppf "%a(%a)" pp_unary op pp_expr e 
    | Op(_, e1, op, e2)                   -> fprintf ppf "(%a %a %a)" pp_expr e1  pp_binary op pp_expr e2
    | Assign(_, v, e)                     -> fprintf ppf "%i := %a" v pp_expr e
    | If(_, e1, e2, e3)                   -> fprintf ppf "if (%a) then (%a) else (%a)" pp_expr e1 pp_expr e2 pp_expr e3
    | While(_, e1, e2)                    -> fprintf ppf "while (%a) do (%a)" pp_expr e1 pp_expr e2
    | Seq (_, [])                         -> () 
    | Seq (_, [e])                        -> pp_expr ppf e 
    | Seq (l, e :: rest)                  -> fprintf ppf "%a; %a" pp_expr e pp_expr (Seq(l, rest))	
    | Lambda (_, l)                       -> pp_lambda ppf l
    | Let (_, x, t, e1, e2)               -> fprintf ppf "let val %s:%a = %a in (%a) end" x pp_type t pp_expr e1 pp_expr e2
    | LetRecFn(_, x, t, l, e)             -> fprintf ppf "let val rec %s:%a = (%a) in (%a) end" x pp_type t pp_lambda l pp_expr e
    | Var(_, x)                           -> fprintf ppf "%s" x
and pp_lambda ppf (l:lambda)=
    match l with
    | (v, t, e) -> fprintf ppf "fn %s:%a => (%a)" v pp_type t pp_expr e

let print_expr e = 
    let _ = pp_expr std_formatter e
    in print_flush () 

let eprint_expr e = 
    let _ = pp_expr err_formatter e
    in print_flush () 

(* useful for degugging *) 


let string_of_uop = function 
  | NEG -> "NEG" 

let string_of_bop = function 
  | ADD  -> "ADD" 
  | MUL  -> "MUL" 
  | DIV  -> "DIV" 
  | SUB  -> "SUB"
  | GTEQ -> "GTEQ"

let mk_con con l = 
    let rec aux carry = function 
      | [] -> carry ^ ")"
      | [s] -> carry ^ s ^ ")"
      | s::rest -> aux (carry ^ s ^ ", ") rest 
    in aux (con ^ "(") l 

let rec string_of_type = function 
  | TEInt             -> "TEInt" 
  | TEBool            -> "TEBool" 
  | TEUnit            -> "TEUnit" 
  | TEArrow(t1, t2)   -> mk_con "TEArrow" [string_of_type t1; string_of_type t2] 
  | TERef(t)          -> mk_con "TERef" [string_of_type t]

let rec string_of_expr = function 
    | Integer (_, n)                      -> mk_con "Integer" [string_of_int n]
    | Skip (_)                            -> mk_con "Skip" []
    | Bool (_, b)                         -> mk_con "Bool" [string_of_bool b]
    | Deref (_, l)                        -> mk_con "Deref" [string_of_int l]
    | App (_, e1, e2)                     -> mk_con "Apply" [string_of_expr e1; string_of_expr e2] 
    | UnaryOp(_, op, e)                   -> mk_con "UnaryOp" [string_of_uop op; string_of_expr e]
    | Op(_, e1, op, e2)                   -> mk_con "Op" [string_of_expr e1; string_of_bop op; string_of_expr e2]
    | Assign(_, l, e)                     -> mk_con "Assign" [string_of_int l; string_of_expr e]
    | If(_, e1, e2, e3)                   -> mk_con "If" [string_of_expr e1; string_of_expr e2; string_of_expr e3]
    | While(_, e1, e2)                    -> mk_con "While" [string_of_expr e1; string_of_expr e2]
    | Seq (_, el)                         -> mk_con "Seq" [string_of_expr_list el]
    | Lambda (_, (x, t, e))               -> mk_con "Lambda" [x; string_of_type t; string_of_expr e]
    | Let (_, x, t, e1, e2)               -> mk_con "Let" [x; string_of_type t; string_of_expr e1; string_of_expr e2]
    | LetRecFn (_, x, t, (y, t1, e1), e)  -> mk_con "LetRecFn" [x; string_of_type t; mk_con "" [y; string_of_type t1; string_of_expr e1]; string_of_expr e]
    | Var(_, x)                           -> mk_con "Var" [x]

and string_of_expr_list = function 
  | [] -> "" 
  | [e] -> string_of_expr e 
  |  e:: rest -> (string_of_expr e ) ^ "; " ^ (string_of_expr_list rest)
