(* Définition des expressions et des opérateurs *)
type expr =
  | Val of int                              
  | Add of expr * expr                     
  | Sub of expr * expr                     
  | Mul of expr * expr                      
  | Div of expr * expr                      
