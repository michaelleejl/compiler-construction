---
header-includes:
 - \usepackage{fvextra}
 - \usepackage{amsmath}
 - \DefineVerbatimEnvironment{Highlighting}{Verbatim}{breaklines,commandchars=\\\{\}}
---
### L2++ Syntax
Booleans $b \in \mathbb{B} = \{\mathbf{true}, \mathbf{false}\}$\
Integers $n \in \mathbb{N} = \{\dots, -1, 0, 1, \dots\}$\
Locations $l \in \mathbb{L} = \{l_0, l_1, \dots\}$. Implemented as strings beginning with "loc_"\
Variables $x \in \mathbb{X} = \{\text{x}, \text{y}, \dots\}$. Implemented as strings. \
Operations $op ::= +|-|\times|\div|\geq$ 

Types
$\begin{split}
T & ::= \text{ int}\mid\text{bool}\mid\text{unit}\mid T_1 \rightarrow T_2\\
T_{loc} & ::= \text{ intref}
\end{split}$
Expressions
$$\begin{split}
e ::= &\, \, \, \, \, n \mid b \mid e_1\  op\  e_2 \mid \mathbf{if}\ e_1\ \mathbf{then}\ e_2\ \mathbf{else}\ e_3 \mid l:=e \mid \,!l
\\
&\mid\mathbf{skip}\mid e_1;e_2\mid \mathbf{begin}\ e\ \mathbf{end} \mid \mathbf{while}\ e_1\ \mathbf{do}\ e_2 \mid x
\\
& \mid \mathbf{fn}\ x:T\Rightarrow e \mid e_1\ e_2 \mid\mathbf{let}\ \mathbf{val}\ x:T = e_1\ \mathbf{in}\ e_2\ \mathbf{end} 
\\& \mid \text{\bf{let val rec}}\ x:T_1 \rightarrow T_2 = (\mathbf{fn}\ y: T_1\Rightarrow e_1)\ \mathbf{in}\ e_2\ \mathbf{end}
\end{split}$$


### Building 
Install `ocamlbuild`.  Then
```ocamlbuild l1.byte```

### Usage 
Usage: l1.byte [options] [<file>]\

Options are:\
    -V verbose front end\
    -v verbose interpreter(s)\
    -c show compiled code (but don't run it)\
    -i0 Interpreter 0\
    -all all interpreters\
    -stackmax set max stack size (default = 1000)\
    -heapmax set max heap size (default = 1000)\
    -t run all test/*.l1 with each selected interpreter, report unexpected outputs (silent otherwise)\
    -help  Display this list of options\
    --help  Display this list of options\


### Files
Every .ml file has an associated .mli file describing its interface. 

errors.ml      : Error exception\
past.ml        : the Parsed AST, with pretty printing\
lexer.mll      : specification for ocamllex\
parser.mly     : specification for ocamlyacc\
ast.ml         : "internal" AST, with pretty printing\
past_to_ast.ml : translated from parsed to internal AST\
static.ml      : static analysis (check types and other rules)\
front_end.ml   : the front end : parse, static check, translate.\
free_vars.ml   : free variable calculation\
tests.ml       : code for parsing tests/manifest.txt and setting up testing.\
l1.ml       : main file, implementing the command-line for the interpreter and compiler\

### Interpreters
_In order of presentation in lectures_\
interp_0.ml    : The "definitional" interpreter. 



               
