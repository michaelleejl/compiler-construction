(*   translate_expr : Past.expr -> Ast.expr 
     
	 Lifted and amended from the original Slang interpreter

*) 

let translate_uop = function 
  | Past.NEG -> Ast.NEG 

let translate_bop = function 
  | Past.ADD -> Ast.ADD 
  | Past.MUL -> Ast.MUL
  | Past.DIV -> Ast.DIV
  | Past.SUB -> Ast.SUB
  | Past.GTEQ -> Ast.GTEQ

let rec translate_expr = function 
    | Past.Integer(_, n)                     -> Ast.Integer n
    | Past.Skip(_)                           -> Ast.Skip
    | Past.Bool(_, b)                        -> Ast.Bool b
    | Past.Deref(_, l)                       -> Ast.Deref l
    | Past.App(_, e1, e2)                    -> Ast.App (translate_expr e1, translate_expr e2)
    | Past.UnaryOp(_, op, e)                 -> Ast.UnaryOp(translate_uop op, translate_expr e)
    | Past.Op(_, e1, op, e2)                 -> Ast.Op(translate_expr e1, translate_bop op, translate_expr e2)
    | Past.Assign(_, l, e)                   -> Ast.Assign (l, translate_expr e)
    | Past.If(_, e1, e2, e3)                 -> Ast.If (translate_expr e1, translate_expr e2, translate_expr e3)
    | Past.While(_, e1, e2)                  -> Ast.While(translate_expr e1, translate_expr e2)
    | Past.Seq(_, e1)                        -> Ast.Seq(List.map translate_expr e1)
    | Past.Lambda(_, (x, t, e))              -> Ast.Lambda (x, translate_expr e)
    | Past.Let(_, x, t, e1, e2)              -> Ast.Let(x, translate_expr e1, translate_expr e2)
    | Past.LetRecFn(_, x, t, (y, t1, e1), e) -> Ast.LetRecFn(x, (y, translate_expr e1), translate_expr e)
    | Past.Var(_, v)                         -> Ast.Var(v)

(*syntactic sugar*)