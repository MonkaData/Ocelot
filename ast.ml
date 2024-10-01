(* Définition des expressions et des opérateurs *)
(* D'après un individu peu fiable il y a de la RECURSION GAUCHE*)
type expr =
  | Val of int                              
  | Add of expr * expr                     
  | Sub of expr * expr                     
  | Mul of expr * expr                      
  | Div of expr * expr                      
