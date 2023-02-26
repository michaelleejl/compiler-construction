(* File lexer.mll *)
{
  open Parser
  open Lexing 
  open String

(* next_line copied from  Ch. 16 of "Real Workd Ocaml" *) 
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let get_location loc =
	String.sub loc 4 (String.length loc)

}


let newline = ('\010' | "\013\010" )
let locat_reg_exp = "loc_"['0'-'9' 'A'-'Z' 'a'-'z']* 
let ident_reg_exp = ['A'-'Z' 'a'-'z']+ ['0'-'9' 'A'-'Z' 'a'-'z' '_' '\'']* 
let int_reg_exp = ['0'-'9']+

	rule token = parse
	  | [' ' '\t']     { token lexbuf }     (* skip blanks *)
	  
	  (*operators*)
	  | "=>"		   { DARROW }
	  | "->"           { ARROW }
	  | ":="           { ASSIGN }
	  | ">="		   { GTEQ }
	  | '+'            { ADD }
	  | '-'            { SUB }
	  | '*'            { MUL }
	  | '/'            { DIV }
	  | '='            { EQUALS }
	  | ':'			   { COLON }
	  | '('            { LPAREN }
	  | ')'            { RPAREN }
	  | ';'	           { SEMICOLON }
	  | "!"			   { BANG }

	  (*control flow*)
	  | "while"        { WHILE }
	  | "do"           { DO }
	  
	  | "if"           { IF }
	  | "then"         { THEN }
	  | "else"         { ELSE }
	  
	  (*closures*)
	  | "let val rec"  { LETREC }
	  | "let val"      { LET }
	  | "fn"           { FUN }
	  | "in"           { IN }
	  | "begin"	       { BEGIN }
	  | "end"          { END }
	  
	  (*types*)
	  | "int"          { INTTYPE }
	  | "bool"         { BOOLTYPE }
	  | "unit"         { UNITTYPE }
	  
	  (*values*)
	  | "skip"         { SKIP }
	  | "true"         { TRUE }
	  | "false"        { FALSE }
	  | int_reg_exp    { INT (int_of_string (Lexing.lexeme lexbuf)) }
	  | locat_reg_exp  { LOCAT get_location (Lexing.lexeme lexbuf) }
	  | ident_reg_exp  { IDENT (Lexing.lexeme lexbuf) }

	  (*misc*)
	  | "(*" { comment lexbuf; token lexbuf }
	  | newline { next_line lexbuf; token lexbuf } 
	  | eof { EOF }
	  | _ { Errors.complain ("Lexer : Illegal character " ^ (Char.escaped(Lexing.lexeme_char lexbuf 0)))
}

and comment = parse
  | "*)" { () }
  | newline { next_line lexbuf; comment lexbuf }
  | "(*" {comment lexbuf; comment lexbuf }
  | _ { comment lexbuf } 