/* File parser.mly */

%{

let get_loc = Parsing.symbol_start_pos 

%}

%token <int> INT
%token <bool> BOOL
%token<string> IDENT
%token SKIP 
%token ADD SUB MUL DIV GEQ ASSIGN DEREF
%token IF THEN ELSE WHILE DO
%token SEMICOLON
%token LPAREN RPAREN
%token EOF

%left ADD SUB        /* lowest precedence */
%left MUL DIV         /* medium precedence */

%right ASSIGN

%nonassoc SKIP BOOL INT LPAREN IDENT       
%nonassoc DEREF /* highest precedence */


%start main
%type <Past.expr> term 
%type <Past.expr> expr 
%type <Past.expr list> exprlist
%type <Past.expr> main

%%
main:
	expr EOF                { $1 }
;
term:
| INT                                { Past.Integer (get_loc(), $1) }
| BOOL                               { Past.Boolean (get_loc(), $1)}
| SKIP                               { Past.Unit (get_loc())}
| IDENT                              { Past.Var (get_loc(), $1) }
| DEREF IDENT                        { Past.Deref(get_loc(), $2) }
| LPAREN expr RPAREN                 { $2 }

expr:
| term                               	{  $1 }
| expr ADD expr                      	{ Past.Op(get_loc(), $1, Past.ADD, $3) }
| expr SUB expr                      	{ Past.Op(get_loc(), $1, Past.SUB, $3) }
| expr MUL expr                      	{ Past.Op(get_loc(), $1, Past.MUL, $3) }
| expr DIV expr                      	{ Past.Op(get_loc(), $1, Past.DIV, $3) }
| expr GEQ expr                      	{ Past.Op(get_loc(), $1, Past.GEQ, $3) }
| IDENT ASSIGN expr                  	{ Past.Op(get_loc(), $1, Past.ASSIGN, $3) }
| IF expr THEN exprlist ELSE exprlist	{ Past.If(get_loc(), $2, $4, $6) }
| WHILE expr DO exprlist             	{ Past.While(get_loc(), $2, $4) }


exprlist:
|   expr                             { [$1] }
|   expr SEMICOLON exprlist         { $1 :: $3  }


