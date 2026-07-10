%{
open Ast

let rec curry params body =
  match params with
  | [] -> body
  | param :: rest -> Fonction (param, curry rest body)
%}

%token <int> ENTIER
%token <string> IDENT
%token LET REC IN IF THEN ELSE WHILE FOR DO DONE TO FUNCTION NOT
%token AND LAND OR MATCH WITH BAR
%token PLUS MINUS TIMES DIV MOD SUPEGAL INFEGAL SUP INF SEMI LPAREN RPAREN
%token LBRACKET RBRACKET CONS
%token TRUE FALSE UNIT EGAL ARROW EOF

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE
%nonassoc LOWER_THAN_SEMI
%right SEMI
%nonassoc LOWER_THAN_BAR
%right BAR

%start programme
%type <Ast.fonction list> programme

%%

programme:
  | fonctions EOF { List.rev $1 }

fonctions:
  | { [] }
  | fonctions LET toplevel_bindings { List.rev_append $3 $1 }
  | fonctions LET REC toplevel_bindings { List.rev_append $4 $1 }

toplevel_bindings:
  | toplevel_binding { [$1] }
  | toplevel_binding AND toplevel_bindings { $1 :: $3 }

toplevel_binding:
  | IDENT params EGAL expr_seq { { nom = $1; params = $2; corps = $4 } }
  | UNIT EGAL expr_seq { { nom = "()"; params = []; corps = $3 } }

params:
  | { [] }
  | IDENT params { $1 :: $2 }

expr_seq:
  | expr %prec LOWER_THAN_SEMI { $1 }
  | expr SEMI expr_seq {
      match $3 with
      | Sequence rest -> Sequence ($1 :: rest)
      | last -> Sequence [$1; last]
    }

expr:
  | LET bindings IN expr_seq { Let (false, $2, $4) }
  | LET REC bindings IN expr_seq { Let (true, $3, $5) }
  | IF expr THEN expr %prec LOWER_THAN_ELSE { Si ($2, $4, None) }
  | IF expr THEN expr ELSE expr { Si ($2, $4, Some $6) }
  | WHILE expr DO expr_seq DONE { TantQue ($2, $4) }
  | FOR IDENT EGAL expr TO expr DO expr_seq DONE { Pour ($2, $4, $6, $8) }
  | FUNCTION IDENT ARROW expr { Fonction ($2, $4) }
  | MATCH expr WITH optional_bar match_cases { Match ($2, $5) }
  | logical_expr { $1 }

optional_bar:
  | { () }
  | BAR { () }

logical_expr:
  | logical_expr OR and_expr { Binop (Ou, $1, $3) }
  | and_expr { $1 }

and_expr:
  | and_expr LAND cons_expr { Binop (Et, $1, $3) }
  | cons_expr { $1 }

cons_expr:
  | equality_expr CONS cons_expr { Cons ($1, $3) }
  | equality_expr { $1 }

equality_expr:
  | additive_expr EGAL additive_expr { Binop (Egal, $1, $3) }
  | additive_expr SUP additive_expr { Binop (Sup, $1, $3) }
  | additive_expr INF additive_expr { Binop (Inf, $1, $3) }
  | additive_expr SUPEGAL additive_expr { Binop (Supegal, $1, $3) }
  | additive_expr INFEGAL additive_expr { Binop (Infegal, $1, $3) }
  | additive_expr { $1 }

additive_expr:
  | additive_expr PLUS multiplicative_expr { Binop (Plus, $1, $3) }
  | additive_expr MINUS multiplicative_expr { Binop (Moins, $1, $3) }
  | multiplicative_expr { $1 }

multiplicative_expr:
  | multiplicative_expr TIMES unary_expr { Binop (Fois, $1, $3) }
  | multiplicative_expr DIV unary_expr { Binop (Divise, $1, $3) }
  | multiplicative_expr MOD unary_expr { Binop (Modulo, $1, $3) }
  | unary_expr { $1 }

unary_expr:
  | NOT unary_expr { Unop (Non, $2) }
  | MINUS unary_expr { Binop (Moins, Entier 0, $2) }
  | app_expr { $1 }

app_expr:
  | app_expr primary_expr { Application ($1, $2) }
  | primary_expr { $1 }

primary_expr:
  | LPAREN expr_seq RPAREN { $2 }
  | ENTIER { Entier $1 }
  | TRUE { Booleen true }
  | FALSE { Booleen false }
  | UNIT { Unite }
  | IDENT { Variable $1 }
  | LBRACKET expr_list RBRACKET { Liste $2 }

expr_list:
  | { [] }
  | expr { [$1] }
  | expr SEMI expr_list { $1 :: $3 }

match_cases:
  | match_case %prec LOWER_THAN_BAR { [$1] }
  | match_case BAR match_cases { $1 :: $3 }

match_case:
  | pattern ARROW expr_seq { ($1, $3) }

pattern:
  | simple_pattern CONS pattern { PCons ($1, $3) }
  | simple_pattern { $1 }

simple_pattern:
  | ENTIER { PInt $1 }
  | TRUE { PBool true }
  | FALSE { PBool false }
  | IDENT { if $1 = "_" then PWildcard else PVar $1 }
  | LBRACKET RBRACKET { PNil }
  | LPAREN pattern RPAREN { $2 }

bindings:
  | binding { [$1] }
  | binding AND bindings { $1 :: $3 }

binding:
  | IDENT params EGAL expr { ($1, curry $2 $4) }
%%
