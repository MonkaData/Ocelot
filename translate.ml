(* translate.ml *)

open Ast
open W

  | TBool -> "bool"
  | TList _ -> "int_list*"

(* Traduction d'une expression OCaml en code C *)
let rec translate_expr e is_expr level =
  match e with
  | Entier n -> string_of_int n
  | Booleen b -> if b then "true" else "false"
  | Variable x -> x
  | Binop (op, e1, e2) ->
      "(" ^ translate_expr e1 true level ^ " " ^ string_of_binop op ^ " " ^ translate_expr e2 true level ^ ")"
  | Unop (op, e1) ->
      string_of_unop op ^ translate_expr e1 true level
  | Liste elems ->
      let rec build = function
        | [] -> "NULL"
        | h :: t -> "cons_int(" ^ translate_expr h true level ^ ", " ^ build t ^ ")"
      in
      build elems
  | Cons (e1, e2) ->
      "cons_int(" ^ translate_expr e1 true level ^ ", " ^ translate_expr e2 true level ^ ")"
  | Si (cond, e_then, None) ->
      if is_expr then
        "(( " ^ translate_expr cond true level ^ " ) ? " ^ translate_expr e_then true level ^ " : 0)"
      else
        indent level ("if (" ^ strip_outer_parens (translate_expr cond true level) ^ ") {\n"
                      ^ indent (level+1) ("return " ^ translate_expr e_then false (level+1) ^ ";") ^ "\n"
                      ^ indent level "}")
  | Si (cond, e_then, Some e_else) ->
      if is_expr then
        "(( " ^ translate_expr cond true level ^ " ) ? " ^ translate_expr e_then true level ^ " : " ^ translate_expr e_else true level ^ ")"
      else
        indent level ("if (" ^ strip_outer_parens (translate_expr cond true level) ^ ") {\n"
                      ^ indent (level+1) ("return " ^ translate_expr e_then false (level+1) ^ ";") ^ "\n"
                      ^ indent level "} else {\n"
                      ^ indent (level+1) ("return " ^ translate_expr e_else false (level+1) ^ ";") ^ "\n"
                      ^ indent level "}")
  | TantQue (cond, body) ->
      indent level ("while (" ^ translate_expr cond true level ^ ") {\n"
                    ^ translate_expr body false (level+1) ^ "\n"
                    ^ indent level "}")
  | Pour (x, e1, e2, body) ->
      indent level ("for (int " ^ x ^ " = " ^ translate_expr e1 true level ^ "; "
                    ^ x ^ " <= " ^ translate_expr e2 true level ^ "; " ^ x ^ "++) {\n"
                    ^ translate_expr body false (level+1) ^ "\n"
                    ^ indent level "}")
  | Match (e_match, cases) ->
      (* Version fusionnée : on stocke e_match dans temp, puis on génère un ternaire si is_expr = true,
         sinon un bloc if/{return…} *)
      let temp = "__match" ^ string_of_int level in
      let value_code = translate_expr e_match true level in

      let rec build cases =
        match cases with
        | [] ->
            if is_expr then
              (* Valeur “par défaut” en cas de match raté : 0 *)
              indent (level+2) "0"
            else
              indent (level+1) "/* match failure */"
        | (pat, expr) :: rest ->
            let cond, binds = translate_pattern pat temp (level+2) in
            if is_expr then
              (* Contexte expression : ternaire *)
              let expr_code = translate_expr expr true (level+2) in
              let then_part = binds ^ expr_code in
              let else_part =
                if rest = [] then "0"
                else strip_outer_parens (build rest)
              in
              indent (level+2) (cond ^ " ? (" ^ then_part ^ ") : " ^ else_part)
            else
              (* Contexte instruction : bloc if/return *)
              let body_code = translate_expr expr false (level+2) in
              let branch =
                indent (level+1)
                  ("if (" ^ cond ^ ") {\n"
                   ^ binds
                   ^ body_code ^ "\n"
                   ^ indent (level+1) "}")
              in
              if rest = [] then
                branch
              else
                branch ^ " else\n" ^ build rest
      in

      if is_expr then
        (* On enveloppe le tout dans ({ …; expr; }) pour que ça soit une expression GNU C *)
        "("
        ^ indent (level) ("{ int_list* " ^ temp ^ " = " ^ value_code ^ "; ")
        ^ strip_outer_parens (build cases)
        ^ " })"
      else
        (* Bloc C classique *)
        indent (level)
          ("{\n"
           ^ indent (level+1) ("int_list* " ^ temp ^ " = " ^ value_code ^ ";\n")
           ^ build cases ^ "\n"
           ^ indent level "}")

  | Let (is_recursive, bindings, expr_in_ocaml) ->
      (* … reste inchangé … *)

  | Sequence exprs ->
      translate_sequence exprs level

  | Fonction (_, _) ->
      failwith "Traduction des fonctions anonymes non supportée dans cette version."

  | Application (Variable "print_int", e) ->
      "printf(\"%d\", " ^ translate_expr e true level ^ ");"

  | Application (e1, e2) ->
      translate_expr e1 true level ^ "(" ^ translate_expr e2 true level ^ ")"
