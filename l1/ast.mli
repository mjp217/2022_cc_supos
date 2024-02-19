
type var = string 

type oper = ADD | MUL | DIV | SUB | GREQ

type unary_oper = NEG

type ident = ID of string

type expr = 
| Integer of int
| Bool of bool
| Ident of ident
| UnaryOp of unary_oper * expr
| Op of expr * oper * expr
| Seq of (expr list)
| If of expr * expr * expr
| While of(expr list) * (expr list)
| Assign of ident * expr
| Deref of ident

(* printing *) 
val string_of_unary_oper : unary_oper -> string 
val string_of_oper : oper -> string 
val string_of_uop : unary_oper -> string 
val string_of_bop : oper -> string 
val print_expr : expr -> unit 
val eprint_expr : expr -> unit
val string_of_expr : expr -> string 
