(* using the structure from the Slang interpreter *)

open Past 

let complain = Errors.complain

let internal_error msg = complain ("INTERNAL ERROR: " ^ msg) 

let report_expecting e msg t = 
    let loc = loc_of_expr e in 
    let loc_str = string_of_loc loc in 
    let e_str = string_of_expr e in 
    let t_str = string_of_type t in 
    complain ("ERROR at location " ^ 
	      loc_str ^ "\nExpression " ^ e_str ^ 
	      "\nhas type " ^ t_str ^ ", but expecting " ^ msg) 

let report_types_not_equal loc t1 t2 = 
    let loc_str = string_of_loc loc in 
    let t1_str = string_of_type t1 in 
    let t2_str = string_of_type t2 in 
    complain ("Error near location " ^ loc_str ^ 
              "\nExpecting type " ^ t1_str ^ " to be equal to type " ^ t2_str)

let report_type_mismatch (e1, t1) (e2, t2) = 
    let loc1 = loc_of_expr e1 in 
    let loc2 = loc_of_expr e2 in 
    let loc1_str = string_of_loc loc1 in 
    let loc2_str = string_of_loc loc2 in 
    let e1_str = string_of_expr e1 in 
    let e2_str = string_of_expr e2 in 
    let t1_str = string_of_type t1 in 
    let t2_str = string_of_type t2 in 
    complain ("ERROR, Type Mismatch: expecting equal types, however\n" ^ 
	      "at location " ^ loc1_str ^ "\nexpression " ^ e1_str ^ "\nhas type " ^ t1_str ^ 
	      " and at location " ^ loc2_str ^ "\nexpression " ^ e2_str ^ "\nhas type " ^ t2_str)

let rec find loc x = function 
  | [] -> complain (x ^ " is not defined at " ^ (string_of_loc loc)) 
  | (y, v) :: rest -> if x = y then v else find loc x rest

  let rec find_int loc x = function 
  | [] -> complain ((string_of_int x) ^ " is not defined at " ^ (string_of_loc loc)) 
  | (y, v) :: rest -> if x = y then v else find_int loc x rest

(* may want to make this more interesting someday ... *) 
let rec match_types (t1, t2) = (t1 = t2) 

let make_uop loc uop (e, t) = 
    match uop, t with 
    | NEG, TEInt  -> (UnaryOp(loc, uop, e), t) 
    | NEG, t'     -> report_expecting e "integer" t

let make_if loc (e1, t1) (e2, t2) (e3, t3) = 
    match t1 with 
    | TEBool -> 
            if match_types (t2, t3) 
             then (If(loc, e1, e2, e3), t2) 
             else report_type_mismatch (e2, t2) (e3, t3) 
         | ty -> report_expecting e1 "boolean" ty 

let make_while loc (e1, t1) (e2, t2) =
    match (t1, t2) with
    | (TEBool, TEUnit) -> (While(loc, e1, e2), t2)
    | (TEBool, ty) -> report_expecting e2 "unit" ty
    | (tx, _) -> report_expecting e1 "bool" tx

let make_bop loc bop (e1, t1) (e2, t2) = 
    match bop, t1, t2 with 
    | ADD, TEInt,  TEInt  -> (Op(loc, e1, bop, e2), t1) 
    | ADD, TEInt,  t      -> report_expecting e2 "integer" t
    | ADD, t,      _      -> report_expecting e1 "integer" t
    | SUB, TEInt,  TEInt  -> (Op(loc, e1, bop, e2), t1) 
    | SUB, TEInt,  t      -> report_expecting e2 "integer" t
    | SUB, t,      _      -> report_expecting e1 "integer" t
    | MUL, TEInt,  TEInt  -> (Op(loc, e1, bop, e2), t1) 
    | MUL, TEInt,  t      -> report_expecting e2 "integer" t
    | MUL, t,      _      -> report_expecting e1 "integer" t
    | DIV, TEInt,  TEInt  -> (Op(loc, e1, bop, e2), t1) 
    | DIV, TEInt,  t      -> report_expecting e2 "integer" t
    | DIV, t,      _      -> report_expecting e1 "integer" t
    | GTEQ, TEInt, TEInt  -> (Op(loc, e1, bop, e2), TEBool)
    | GTEQ, TEInt,  t     -> report_expecting e2 "integer" t
    | GTEQ, t,      _     -> report_expecting e1 "integer" t

let make_assign loc l (e, t) store =
    let tref = find_int loc l store in
    match tref with
    | TERef tr -> if match_types (t, tr) then (Assign(loc, l, e), TEUnit)
                  else let loc_str = string_of_loc loc in
                  let t_str = string_of_type t in 
                  let tr_str = string_of_type tr in
                  let e_str = string_of_expr e in
                  complain ("ERROR, Type Mismatch: expecting equal types, however\n" ^ 
                  "l" ^ "\nhas type " ^ tr_str ^ 
                  " and at location " ^ loc_str ^ "\nexpression " ^ e_str ^ "\nhas type " ^ t_str)

let make_deref loc l store =
    let tref = find_int loc l store in
    match tref with
        | TERef tr -> (Deref(loc, l), tr)

let make_lambda loc x t (e, t1) = (Lambda (loc, (x, t, e)), TEArrow(t, t1))

let make_apply loc (e1, t1) (e2, t2) =
    match t1 with
    | TEArrow(tx, ty) -> if match_types (tx, t2) then (App(loc, e1, e2), ty)
                         else 
                            match e1 with
                            | Lambda(loc, (x, t, e)) -> report_type_mismatch (Var(loc, x), tx) (e2, t2)
    

let make_let loc x t (e1, t1) (e2, t2)  = 
    if match_types (t, t1) 
    then (Let(loc, x, t, e1, e2), t2)
    else report_types_not_equal loc t t1 

let make_letrecfun loc x y t1 (e1, t2) (e, t) = (LetRecFn(loc, x, t, (y, t1, e1), e), t)


let rec infer env store e = 
    match e with 
    | Integer _            -> (e, TEInt)
    | Bool _               -> (e, TEBool)
    | Skip _               -> (e, TEUnit)
    | UnaryOp(loc, uop, e) -> make_uop loc uop (infer env store e) 
    | Op(loc, e1, bop, e2) -> make_bop loc bop (infer env store e1) (infer env store e2) 
    | Seq(loc, el)         -> infer_seq loc env store el 
    | If(loc, e1, e2, e3)  -> make_if loc (infer env store e1) (infer env store e2) (infer env store e3) 
    | While(loc, e1, e2)   -> make_while loc (infer env store e1) (infer env store e2)
    | Assign(loc, l, e)    -> make_assign loc l (infer env store e) store
    | Deref(loc, l)        -> make_deref loc l store
    | Lambda(loc, (x, t, e)) -> make_lambda loc x t (infer ((x, t)::env) store e)
    | App(loc, e1, e2)   -> make_apply loc (infer env store e1) (infer env store e2)
    | Var(loc, x)        -> (e, find loc x env)
    | Let (loc, x, t, e1, e2) -> make_let loc x t (infer env store e1) (infer ((x, t)::env) store e2)
    | LetRecFn(loc, x, t, (y, t1, e1), e) -> 
      let env1 = (x, t) :: env in 
      let p = infer env1 store e  in 
      let env2 = (y, t1) :: env in 
      let env3 = (x, t) :: env2 in 
      make_letrecfun loc x y t1 (infer env3 store e1) p

and infer_seq loc env store el = 
    let rec aux carry = function 
      | []        -> internal_error "empty sequence found in parsed AST" 
      | [e]       -> let (e', t) = infer env store e in (Seq(loc, List.rev (e' :: carry )), t)
      | e :: rest -> let (e', _) = infer env store e in aux (e' :: carry) rest 
    in aux [] el 

let env_init = [] 

let store_init = [(1, TERef(TEInt)); (2, TERef(TEInt)); (3, TERef(TEInt))]

let check e = 
    let (e', _) = infer env_init store_init e 
    in e' 

