/* parser.mly */

/*
 * Ce fichier définit la grammaire pour les expressions arithmétiques 
 * que notre transpileur va analyser. Nous utilisons Menhir comme générateur
 * d'analyseur syntaxique. La grammaire est définie en utilisant la notation 
 * BNF étendue, où chaque règle peut avoir plusieurs alternatives.
 *
 * Nous allons définir les tokens que le lexer fournit, les règles de grammaire
 * pour les expressions, et les actions qui construisent l'Arbre Syntaxique Abstrait (AST).
 */

%{
(* En-têtes OCaml *)
(* 
   Nous ouvrons le module Ast pour pouvoir utiliser les constructeurs de l'AST directement 
   dans les actions des règles de grammaire.
*)
open Ast
%}

/* 
 * Déclaration des tokens que le lexer reconnaîtra.
 * Chaque token correspond à un élément de l'expression arithmétique.
 */
%token <int> INT           (* Représente un entier, par exemple 42 *)
%token PLUS               (* Représente le symbole '+' *)
%token MINUS              (* Représente le symbole '-' *)
%token TIMES              (* Représente le symbole '*' *)
%token DIVIDE             (* Représente le symbole '/' *)
%token LPAREN             (* Représente le symbole '(' *)
%token RPAREN             (* Représente le symbole ')' *)
%token EOF                (* Représente la fin de l'entrée *)

/* 
 * Déclarations de précédence et d'associativité des opérateurs.
 * Les opérateurs déclarés plus tard dans le fichier ont une précédence plus élevée.
 *
 * - `%left` : Associatif à gauche
 * - `%right` : Associatif à droite
 * - `%nonassoc` : Sans associativité
 *
 * Dans cet exemple :
 * 1. `PLUS` et `MINUS` sont associatifs à gauche et ont une précédence inférieure.
 * 2. `TIMES` et `DIVIDE` sont associatifs à gauche et ont une précédence supérieure.
 * 3. `UMINUS` (opérateur unaire `-`) est associatif à droite et a la plus haute précédence.
 */
%left PLUS MINUS           (* '+' et '-' ont la même priorité et sont associatifs à gauche *)
%left TIMES DIVIDE         (* '*' et '/' ont la même priorité, supérieure à '+' et '-', associatives à gauche *)
%right UMINUS              (* Définition d'un opérateur unaire '-' avec priorité supérieure *)

%start main                 (* Le point d'entrée de la grammaire *)
%type <Ast.expr> main        (* Le type de retour du point d'entrée est `Ast.expr` *)

%%

/*
 * Section des règles de grammaire.
 * Chaque règle définit comment un non-terminal peut être construit à partir de 
 * combinaisons de tokens ou d'autres non-terminaux.
 */

/*
 * La règle `main` est le point d'entrée de l'analyseur syntaxique.
 * Elle attend une expression suivie de la fin de l'entrée (EOF).
 * L'action `{ $1 }` signifie que le résultat de `main` est simplement l'expression analysée.
 */
main:
  | expr EOF { $1 }

/*
 * La règle `expr` définit les différentes façons dont une expression peut être construite.
 * Elle inclut les opérations binaires (addition, soustraction, multiplication, division),
 * l'opérateur unaire (négation), les parenthèses, et les entiers.
 *
 * Les actions `{ Add ($1, $3) }`, etc., construisent les nœuds correspondants de l'AST.
 */
expr:
  | expr PLUS expr         { Add ($1, $3) }
      (* Addition : expr + expr *)
  | expr MINUS expr        { Sub ($1, $3) }
      (* Soustraction : expr - expr *)
  | expr TIMES expr        { Mul ($1, $3) }
      (* Multiplication : expr * expr *)
  | expr DIVIDE expr       { Div ($1, $3) }
      (* Division : expr / expr *)
  | MINUS expr %prec UMINUS { Neg ($2) }
      (* Négation unaire : -expr
         L'opérateur `%prec UMINUS` indique que cette règle utilise la précédence 
         de `UMINUS` définie précédemment, pour éviter les ambiguïtés avec la soustraction binaire. *)
  | LPAREN expr RPAREN     { $2 }
      (* Parenthèses : (expr)
         L'action `{ $2 }` retourne simplement l'expression à l'intérieur des parenthèses, 
         sans ajouter de nœud supplémentaire à l'AST. *)
  | INT                    { Int $1 }
      (* Entier : un nombre entier, par exemple 42
         L'action `{ Int $1 }` crée un nœud `Int` dans l'AST avec la valeur de l'entier. *)
