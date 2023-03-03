/* File parser.mly */

%{

let get_loc = Parsing.symbol_start_pos 

%}


%token <int> INT
%token <string> IDENT
%token <string> LOCAT
%token SKIP TRUE FALSE
%token ADD SUB MUL DIV EQUALS GTEQ ASSIGN 
%token LPAREN RPAREN SEMICOLON COLON ARROW BANG DARROW
%token END WHILE DO IF THEN ELSE LAMBDA LET IN LETREC FUN BEGIN
%token INTTYPE BOOLTYPE UNITTYPE
%token EOF

/* Associativities for binary operations
   ADD, SUB, MUL, DIV, EQUALS, GTEQ, ASSIGN, SEMICOLON, COLON
*/

%nonassoc LET LETREC LAMBDA FUN IN
%nonassoc WHILE DO IF
%nonassoc THEN
%nonassoc ELSE
%right ARROW
%right DARROW
%left EQUALS /* let x=e1;e2;e3 in e === let x=(e1;e2;e3) in e*/
%right ASSIGN
%left SEMICOLON /* left: e1 ; e2; e3 === (e1;e2);e3 */
%left GTEQ 
%left ADD SUB        
%left MUL DIV       
%nonassoc IDENT
%nonassoc UMINUS

%nonassoc INT SKIP TRUE FALSE LPAREN BANG LOCAT
%nonassoc EOF

%start main
%type <Past.expr> simple_expr 
%type <Past.expr> expr 
%type <Past.expr list> exprlist
%type <Past.type_expr> typexpr 
%type <Past.expr> main

%%
main:
	expr EOF               							{ $1 }
;

simple_expr:
  INT                                				{ Past.Integer (get_loc(), $1) }
| SKIP  							 				{ Past.Skip (get_loc()) }
| TRUE								 				{ Past.Bool (get_loc(), true) }
| FALSE								 				{ Past.Bool (get_loc(), false) }
| LPAREN expr RPAREN                 				{ $2 }


expr:
  IDENT						   	     				{ Past.Var (get_loc(), $1) }
| BANG LOCAT						 				{ Past.Deref (get_loc(), $2) }
| simple_expr                        				{ $1 }
| SUB expr %prec INT                		        	{ Past.UnaryOp(get_loc(), Past.NEG, $2) } 
| expr simple_expr 						 			{ Past.App(get_loc(), $1, $2) }
| expr ADD expr                      				{ Past.Op(get_loc(), $1, Past.ADD,  $3) }
| expr SUB expr                      				{ Past.Op(get_loc(), $1, Past.SUB,  $3) }
| expr MUL expr                      				{ Past.Op(get_loc(), $1, Past.MUL,  $3) }
| expr DIV expr                      				{ Past.Op(get_loc(), $1, Past.DIV,  $3) }
| expr GTEQ expr					 				{ Past.Op(get_loc(), $1, Past.GTEQ, $3) }
| LOCAT ASSIGN expr					 				{ Past.Assign(get_loc(), $1, $3) }
| IF expr THEN expr ELSE expr 	 	 				{ Past.If(get_loc(), $2, $4, $6) }
| WHILE expr DO expr                 				{ Past.While(get_loc(), $2, $4) }
| BEGIN exprlist END								{ Past.Seq(get_loc(), $2) }
| FUN IDENT COLON typexpr DARROW expr  				{ Past.Lambda(get_loc(), ($2, $4, $6)) }
| LET IDENT COLON typexpr EQUALS expr IN expr END   { Past.Let(get_loc(), $2, $4, $6, $8) }
| LETREC IDENT COLON typexpr EQUALS LPAREN FUN IDENT COLON typexpr DARROW expr RPAREN IN expr END  { Past.LetRecFn(get_loc(), $2, $4, ($8, $10, $12), $15) }

exprlist:
  expr SEMICOLON exprlist							{ $1::$3 }

typexpr:
  INTTYPE							 { Past.TEInt }
| BOOLTYPE							 { Past.TEBool }
| UNITTYPE							 { Past.TEUnit }
| typexpr ARROW typexpr              { Past.TEArrow ($1, $3)}

