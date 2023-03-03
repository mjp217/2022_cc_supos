/* File parser.mly */

%{

let get_loc = Parsing.symbol_start_pos 

%}

%token <int> INT
%token <bool> BOOL
%token <string> ID
%token ADD SUB MUL DIV GREQ
%token LPAREN RPAREN
%token IF THEN ELSE
%token ASSIGN
%token DEREF
%token SKIP
%token SEMICOLON
%token WHILE DO
%token EOF
%left ADD SUB        /* lowest precedence */
%left MUL DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
%nonassoc GREQ        /* highest precedence */
%nonassoc ASSIGN        /* highest precedence */
%nonassoc DEREF        /* highest precedence */


%start main
%type <Past.expr> simple_expr 
%type <Past.expr> expr 
%type <Past.expr list> exprlist
%type <Past.expr> main

%%
main:
	expr EOF                { $1 }
;
simple_expr:
| INT                                { Past.Integer (get_loc(), $1) }
| BOOL                               { Past.Bool (get_loc(), $1) }
// | LPAREN expr RPAREN                 { $2 }

ident:
| ID 								 { Past.Ident (get_loc(), Past.ID($1)) }

expr:
| simple_expr                        {  $1 }
| expr ADD expr                      { Past.Op(get_loc(), $1, Past.ADD, $3) }
| expr SUB expr                      { Past.Op(get_loc(), $1, Past.SUB, $3) }
| expr MUL expr                      { Past.Op(get_loc(), $1, Past.MUL, $3) }
| expr DIV expr                      { Past.Op(get_loc(), $1, Past.DIV, $3) }
| expr GREQ expr                      { Past.Op(get_loc(), $1, Past.GREQ, $3) }
| LPAREN exprlist RPAREN                 { Past.Seq(get_loc(), $2) }
| IF expr THEN expr ELSE expr  { Past.If(get_loc(), $2, $4, $6) }
| WHILE exprlist DO exprlist               { Past.While(get_loc(), $2, $4) }
// | ident ASSIGN expr                   { Past.Assign(get_loc(), $1, $3) }
// | DEREF ident                         { Past.Deref(get_loc(), $2) }

exprlist:
|   expr                             { [$1] }
|   expr  SEMICOLON exprlist         { $1 :: $3  }


