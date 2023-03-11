type var = string
type store_location = int
type loc = Lexing.position
type type_expr = TEInt | TEBool | TEUnit | TEArrow of type_expr * type_expr | TERef of type_expr
type formals = (var * type_expr) list
type oper = ADD | MUL | DIV | SUB | GTEQ
type unary_oper = NEG
type expr =
    Integer of loc * int
  | Skip of loc
  | Bool of loc * bool
  | Deref of loc * store_location
  | App of loc * expr * expr
  | UnaryOp of loc * unary_oper * expr
  | Op of loc * expr * oper * expr
  | Assign of loc * store_location * expr
  | If of loc * expr * expr * expr
  | While of loc * expr * expr
  | Seq of loc * expr list
  | Lambda of loc * lambda
  | Let of loc * var * type_expr * expr * expr
  | LetRecFn of loc * var * type_expr * lambda * expr
  | Var of loc * var
and lambda = var * type_expr * expr
val loc_of_expr : expr -> loc
val string_of_loc : Lexing.position -> string
val pp_uop : unary_oper -> string
val pp_bop : oper -> string
val string_of_oper : oper -> string
val string_of_unary_oper : unary_oper -> string
val fstring : Format.formatter -> string -> unit
val pp_type : Format.formatter -> type_expr -> unit
val pp_unary : Format.formatter -> unary_oper -> unit
val pp_binary : Format.formatter -> oper -> unit
val pp_expr : Format.formatter -> expr -> unit
val pp_lambda : Format.formatter -> lambda -> unit
val print_expr : expr -> unit
val eprint_expr : expr -> unit
val string_of_uop : unary_oper -> string
val string_of_bop : oper -> string
val mk_con : string -> string list -> string
val string_of_type : type_expr -> string
val string_of_expr : expr -> var
val string_of_expr_list : expr list -> var
