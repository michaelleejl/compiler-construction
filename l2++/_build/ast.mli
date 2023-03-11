
type var = string
type loc = int
type oper = ADD | MUL | DIV | SUB | GTEQ
type unary_oper = NEG
type expr =
    Integer of int
  | Skip
  | Bool of bool
  | Deref of loc
  | App of expr * expr
  | UnaryOp of unary_oper * expr
  | Op of expr * oper * expr
  | Assign of loc * expr
  | If of expr * expr * expr
  | While of expr * expr
  | Seq of expr list
  | Lambda of lambda
  | LetRecFn of var * lambda * expr
  | Var of var
and lambda = var * expr
val pp_uop : unary_oper -> string
val pp_bop : oper -> string
val string_of_oper : oper -> string
val string_of_unary_oper : unary_oper -> string
val fstring : Format.formatter -> string -> unit
val pp_unary : Format.formatter -> unary_oper -> unit
val pp_binary : Format.formatter -> oper -> unit
val pp_expr : Format.formatter -> expr -> unit
val pp_expr_list : Format.formatter -> expr list -> unit
val pp_lambda : Format.formatter -> lambda -> unit
val print_expr : expr -> unit
val eprint_expr : expr -> unit
val string_of_uop : unary_oper -> string
val string_of_bop : oper -> string
val mk_con : string -> string list -> string
val string_of_expr : expr -> var
val string_of_expr_list : expr list -> var