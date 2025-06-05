(* ast.ml *)

type ident = string (* Nom de chaque variable, fonction, paramètre déclaré *)

type operateur_binaire = 
  | Plus
  | Moins
  | Fois
  | Divise
  | Modulo
  | Et
  | Ou
  | Egal
  | Inf 
  | Sup 
  | Infegal
  | Supegal

type operateur_unaire =
  | Non

type expr =
  | Entier of int
  | Booleen of bool
  | Variable of ident
  | Binop of operateur_binaire * expr * expr
  | Unop of operateur_unaire * expr
  | Si of expr * expr * expr option   (* Si condition alors expr1 [sinon expr2] *)
  | Let of bool * (ident * expr) list * expr (* let [rec] (id1 = expr1 and id2 = expr2 ...) in expr_in *)
  | Fonction of ident * expr            (* fonction anonyme : fun id -> expr *)
  | Application of expr * expr          (* appel de fonction *)
  | TantQue of expr * expr              (* while condition do expr *)
  | Pour of ident * expr * expr * expr   (* for id = expr1 to expr2 do expr *)
  | Sequence of expr list               (* séquence d'expressions séparées par ";" *)

type fonction = {
  nom : ident;
  params : ident list;
  corps : expr;
}
