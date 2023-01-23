// Header.
// The code here will be copied literally into the generated [parser.ml] file.
// Here we use it just to open the Ast module so that, later on in the grammar
// definition, we can write expressions like [Int i] instead of [Ast.Int i].
// If we wanted, we could also define some OCaml functions in the header.
%{
open Ast
%}

// Declarations.
%token <int> INT
%token <string> ID
%token TRUE
%token FALSE
%token LEQ
%token TIMES  
%token PLUS
%token LPAREN
%token RPAREN
%token LET
%token EQUALS
%token IN
%token IF
%token THEN
%token ELSE
%token EOF

// Additional information about precedence and associativity.
// Lower declaration has higher-level precedence.
// For example, 1 + 2 + 3 will parse as [(1 + 2) + 3] and not as [1 + (2 + 3)].
// [let x = 1 in x + 2] will parse as [let x = 1 in (x + 2)] and not as [(let x = 1 in x) + 2].
%nonassoc IN
%nonassoc ELSE
%left LEQ
%left PLUS
%left TIMES  

// The following declaration says to start with a rule (defined below) named [prog].
// The declaration also says that parsing a [prog] will return an OCaml value of type [Ast.expr].
%start <Ast.expr> prog

// End of the declarations section.
%%

// Rules.
// Like BNF and pattern matching.
prog:
	| e = expr; EOF { e }
	;
	
expr:
	| i = INT { Int i }
	| x = ID { Var x }
	| TRUE { Bool true }
	| FALSE { Bool false }
	| e1 = expr; LEQ; e2 = expr { Binop (Leq, e1, e2) }
	| e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) } 
	| e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
	| LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
	| IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
	| LPAREN; e=expr; RPAREN {e} 
	;
	
// Maybe have a [trailer] section after the rules, which like the header is
// OCaml code that is copied directly into the output parser.ml file.
