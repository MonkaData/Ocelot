open Ast
open W

exception Error of string

type function_info = {
  source_name : string;
  c_name : string;
  parameters : (string * typ) list;
  result_type : typ;
  full_type : typ;
  body : expr;
}

type local = {
  local_source : string;
  local_c_name : string;
  local_type : typ;
}

type context = {
  functions : function_info list;
  locals : local list;
}

type generated = {
  code : string;
  value : string;
  typ : typ;
}

let name_counter = ref 0

let fresh_name prefix =
  let id = !name_counter in
  incr name_counter;
  Printf.sprintf "ocelot_tmp_%s_%d" prefix id

let indent level text = String.make (2 * level) ' ' ^ text
let line level text = indent level text ^ "\n"

let sanitize_identifier name =
  let buffer = Buffer.create (String.length name + 8) in
  String.iter
    (function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' as c -> Buffer.add_char buffer c
      | '\'' -> Buffer.add_string buffer "_prime"
      | _ -> Buffer.add_char buffer '_')
    name;
  "ocelot_user_" ^ Buffer.contents buffer

let string_of_c_value_type = function
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "int"
  | TList TInt -> "int_list *"
  | TList item ->
      raise
        (Error
           ("seules les listes d'entiers sont traduisibles en C (type reçu : "
            ^ W.string_of_typ (TList item) ^ ")"))
  | TFun _ as typ ->
      raise (Error ("une fonction ne peut pas être utilisée comme valeur C : " ^ W.string_of_typ typ))
  | TVar _ as typ ->
      raise (Error ("type ambigu impossible à traduire : " ^ W.string_of_typ typ))

let string_of_c_return_type = function
  | TUnit -> "void"
  | typ -> string_of_c_value_type typ

let rec function_type parameter_types result_type =
  match parameter_types with
  | [] -> result_type
  | parameter :: rest -> TFun (parameter, function_type rest result_type)

let rec expression_of_function parameters body =
  match parameters with
  | [] -> body
  | parameter :: rest -> Fonction (parameter, expression_of_function rest body)

let split_function_type name arity typ =
  let rec split remaining arguments typ =
    if remaining = 0 then List.rev arguments, typ
    else
      match typ with
      | TFun (argument, result) -> split (remaining - 1) (argument :: arguments) result
      | _ ->
          raise
            (Error
               (Printf.sprintf
                  "la signature inférée de %s ne contient pas ses %d paramètre(s)"
                  name arity))
  in
  split arity [] typ

let ensure_unique label names =
  let rec check seen = function
    | [] -> ()
    | name :: rest ->
        if List.mem name seen then raise (Error (label ^ " dupliqué : " ^ name));
        check (name :: seen) rest
  in
  check [] names

let infer_toplevel functions main_expression =
  W.reset_fresh_vars ();
  ensure_unique "nom de fonction" (List.map (fun fn -> fn.nom) functions);
  List.iter
    (fun fn -> ensure_unique ("paramètre de " ^ fn.nom) fn.params)
    functions;
  let placeholders =
    List.map (fun fn -> fn.nom, W.fresh_tyvar ()) functions
  in
  let recursive_env =
    List.map (fun (name, typ) -> name, Forall ([], typ)) placeholders
  in
  let rec infer_functions subst = function
    | [] -> subst
    | fn :: rest ->
        let env = W.apply_subst_env subst recursive_env in
        let expression = expression_of_function fn.params fn.corps in
        let expression_subst, inferred_type = W.w env expression in
        let subst = W.compose_subst expression_subst subst in
        let expected = W.apply_subst subst (List.assoc fn.nom placeholders) in
        let signature_subst = W.unify expected (W.apply_subst subst inferred_type) in
        infer_functions (W.compose_subst signature_subst subst) rest
  in
  let subst = infer_functions [] functions in
  let env = W.apply_subst_env subst recursive_env in
  let main_subst, main_type = W.w env main_expression in
  let subst = W.compose_subst main_subst subst in
  let unit_subst = W.unify (W.apply_subst subst main_type) TUnit in
  let subst = W.compose_subst unit_subst subst in
  List.map
    (fun fn ->
      let full_type = W.apply_subst subst (List.assoc fn.nom placeholders) in
      let parameter_types, result_type =
        split_function_type fn.nom (List.length fn.params) full_type
      in
      {
        source_name = fn.nom;
        c_name = sanitize_identifier fn.nom;
        parameters = List.combine fn.params parameter_types;
        result_type;
        full_type;
        body = fn.corps;
      })
    functions

let type_env context =
  let locals =
    List.map
      (fun local -> local.local_source, Forall ([], local.local_type))
      context.locals
  in
  let functions =
    List.map
      (fun fn -> fn.source_name, Forall ([], fn.full_type))
      context.functions
  in
  locals @ functions

let infer context expression = W.infer_expr (type_env context) expression

let find_local context name =
  List.find_opt (fun local -> local.local_source = name) context.locals

let find_function context name =
  List.find_opt (fun fn -> fn.source_name = name) context.functions

let materialize level prefix generated =
  let name = fresh_name prefix in
  let declaration =
    line level
      (string_of_c_value_type generated.typ ^ " " ^ name ^ " = " ^ generated.value ^ ";")
  in
  generated.code ^ declaration, name

let discard level generated =
  generated.code ^ line level ("(void)(" ^ generated.value ^ ");")

let string_of_binop = function
  | Plus -> "+"
  | Moins -> "-"
  | Fois -> "*"
  | Et -> "&&"
  | Ou -> "||"
  | Egal -> "=="
  | Inf -> "<"
  | Sup -> ">"
  | Infegal -> "<="
  | Supegal -> ">="
  | Divise | Modulo -> assert false

let rec flatten_application expression arguments =
  match expression with
  | Application (fn, argument) -> flatten_application fn (argument :: arguments)
  | base -> base, arguments

let rec emit context expression level =
  match expression with
  | Entier value -> { code = ""; value = string_of_int value; typ = TInt }
  | Booleen value ->
      { code = ""; value = if value then "true" else "false"; typ = TBool }
  | Unite -> { code = ""; value = "0"; typ = TUnit }
  | Variable name -> emit_variable context name
  | Binop ((Et | Ou) as operator, left, right) ->
      emit_short_circuit context operator left right level
  | Binop (operator, left, right) -> emit_binop context operator left right level
  | Unop (Non, inner) ->
      let generated = emit context inner level in
      let code, value = materialize level "unary" generated in
      { code; value = "(!" ^ value ^ ")"; typ = TBool }
  | Liste expressions -> emit_list context expressions level
  | Cons (head, tail) -> emit_cons context head tail level
  | Si (condition, then_branch, else_branch) ->
      emit_if context condition then_branch else_branch level
  | Let (true, _, _) ->
      raise
        (Error
           "les fonctions récursives locales ne sont pas encore traduisibles ; déclarez-les au niveau global")
  | Let (false, bindings, body) -> emit_let context bindings body level
  | Fonction _ ->
      raise
        (Error
           "les fonctions anonymes ne sont traduisibles que lorsqu'elles correspondent aux paramètres d'une fonction globale")
  | Application _ -> emit_application context expression level
  | TantQue (condition, body) -> emit_while context condition body level
  | Pour (name, first, last, body) -> emit_for context name first last body level
  | Match (matched, cases) -> emit_match context matched cases level
  | Sequence expressions -> emit_sequence context expressions level

and emit_variable context name =
  match find_local context name with
  | Some local -> { code = ""; value = local.local_c_name; typ = local.local_type }
  | None ->
      (match find_function context name with
       | Some fn when fn.parameters = [] ->
           { code = ""; value = fn.c_name ^ "()"; typ = fn.result_type }
       | Some _ -> raise (Error ("fonction utilisée sans être appelée : " ^ name))
       | None ->
           (match W.builtin_type name with
            | Some _ -> raise (Error ("fonction intégrée utilisée sans être appelée : " ^ name))
            | None -> raise (Error ("identifiant inconnu pendant la traduction : " ^ name))))

and emit_binop context operator left right level =
  let left = emit context left level in
  let left_code, left_value = materialize level "left" left in
  let right = emit context right level in
  let right_code, right_value = materialize level "right" right in
  let result_type =
    match operator with
    | Plus | Moins | Fois | Divise | Modulo -> TInt
    | Egal | Inf | Sup | Infegal | Supegal -> TBool
    | Et | Ou -> assert false
  in
  let value =
    match operator with
    | Divise -> "ocelot_runtime_div(" ^ left_value ^ ", " ^ right_value ^ ")"
    | Modulo -> "ocelot_runtime_mod(" ^ left_value ^ ", " ^ right_value ^ ")"
    | _ -> "(" ^ left_value ^ " " ^ string_of_binop operator ^ " " ^ right_value ^ ")"
  in
  { code = left_code ^ right_code; value; typ = result_type }

and emit_short_circuit context operator left right level =
  let left = emit context left level in
  let left_code, left_value = materialize level "logical_left" left in
  let result = fresh_name "logical_result" in
  let right = emit context right (level + 1) in
  let right_code, right_value = materialize (level + 1) "logical_right" right in
  let condition = match operator with Et -> left_value | Ou -> "!" ^ left_value | _ -> assert false in
  let code =
    left_code
    ^ line level ("bool " ^ result ^ " = " ^ left_value ^ ";")
    ^ line level ("if (" ^ condition ^ ") {")
    ^ right_code
    ^ line (level + 1) (result ^ " = " ^ right_value ^ ";")
    ^ line level "}"
  in
  { code; value = result; typ = TBool }

and emit_list context expressions level =
  let list_type =
    match expressions with
    | [] -> TList TInt
    | _ -> infer context (Liste expressions)
  in
  ignore (string_of_c_value_type list_type);
  let rec evaluate code values = function
    | [] -> code, List.rev values
    | expression :: rest ->
        let generated = emit context expression level in
        let generated_code, value = materialize level "list_item" generated in
        evaluate (code ^ generated_code) (value :: values) rest
  in
  let code, values = evaluate "" [] expressions in
  let value =
    List.fold_right
      (fun item tail -> "ocelot_runtime_cons_int(" ^ item ^ ", " ^ tail ^ ")")
      values "NULL"
  in
  { code; value; typ = list_type }

and emit_cons context head tail level =
  let head = emit context head level in
  let head_code, head_value = materialize level "cons_head" head in
  let tail = emit context tail level in
  let tail_code, tail_value = materialize level "cons_tail" tail in
  let typ = TList head.typ in
  ignore (string_of_c_value_type typ);
  { code = head_code ^ tail_code;
    value = "ocelot_runtime_cons_int(" ^ head_value ^ ", " ^ tail_value ^ ")";
    typ }

and emit_if context condition then_branch else_branch level =
  let result_type = infer context (Si (condition, then_branch, else_branch)) in
  let condition = emit context condition level in
  let condition_code, condition_value = materialize level "condition" condition in
  let result = fresh_name "if_result" in
  let then_generated = emit context then_branch (level + 1) in
  let else_generated =
    match else_branch with
    | None -> { code = ""; value = "0"; typ = TUnit }
    | Some expression -> emit context expression (level + 1)
  in
  let code =
    condition_code
    ^ line level (string_of_c_value_type result_type ^ " " ^ result ^ ";")
    ^ line level ("if (" ^ condition_value ^ ") {")
    ^ then_generated.code
    ^ line (level + 1) (result ^ " = " ^ then_generated.value ^ ";")
    ^ line level "} else {"
    ^ else_generated.code
    ^ line (level + 1) (result ^ " = " ^ else_generated.value ^ ";")
    ^ line level "}"
  in
  { code; value = result; typ = result_type }

and emit_let context bindings body level =
  let result_type = infer context (Let (false, bindings, body)) in
  let result = fresh_name "let_result" in
  let generated_bindings =
    List.map
      (fun (name, expression) ->
        let generated = emit context expression (level + 1) in
        let typ = infer context expression in
        let local =
          { local_source = name; local_c_name = fresh_name (sanitize_identifier name); local_type = typ }
        in
        generated, local)
      bindings
  in
  let body_context =
    { context with locals = List.map snd generated_bindings @ context.locals }
  in
  let body = emit body_context body (level + 1) in
  let bindings_code =
    generated_bindings
    |> List.map (fun (generated, local) ->
         generated.code
         ^ line (level + 1)
             (string_of_c_value_type local.local_type ^ " " ^ local.local_c_name
              ^ " = " ^ generated.value ^ ";"))
    |> String.concat ""
  in
  let code =
    line level (string_of_c_value_type result_type ^ " " ^ result ^ ";")
    ^ line level "{"
    ^ bindings_code
    ^ body.code
    ^ line (level + 1) (result ^ " = " ^ body.value ^ ";")
    ^ line level "}"
  in
  { code; value = result; typ = result_type }

and emit_application context expression level =
  let base, arguments = flatten_application expression [] in
  match base with
  | Variable "print_int" -> emit_builtin_print context "int" arguments level
  | Variable "print_bool" -> emit_builtin_print context "bool" arguments level
  | Variable "print_newline" ->
      if List.length arguments <> 1 then
        raise (Error "print_newline attend exactement l'argument ()");
      let argument = emit context (List.hd arguments) level in
      let code = discard level argument ^ line level "putchar('\\n');" in
      { code; value = "0"; typ = TUnit }
  | Variable name ->
      (match find_function context name with
       | None -> raise (Error ("appel d'une fonction inconnue : " ^ name))
       | Some fn -> emit_function_call context fn arguments level)
  | _ ->
      raise
        (Error
           "les appels indirects et les valeurs fonctionnelles ne sont pas encore traduisibles")

and emit_builtin_print context kind arguments level =
  if List.length arguments <> 1 then
    raise (Error ("print_" ^ kind ^ " attend exactement un argument"));
  let argument = emit context (List.hd arguments) level in
  let code, value = materialize level ("print_" ^ kind) argument in
  let statement =
    if kind = "int" then "printf(\"%d\", " ^ value ^ ");"
    else "fputs(" ^ value ^ " ? \"true\" : \"false\", stdout);"
  in
  { code = code ^ line level statement; value = "0"; typ = TUnit }

and emit_function_call context fn arguments level =
  if List.length arguments <> List.length fn.parameters then
    raise
      (Error
         (Printf.sprintf "%s attend %d argument(s), %d reçu(s)"
            fn.source_name (List.length fn.parameters) (List.length arguments)));
  let rec evaluate code values parameters arguments =
    match parameters, arguments with
    | [], [] -> code, List.rev values
    | (_, expected_type) :: parameters, argument :: arguments ->
        let generated = emit context argument level in
        let value_name = fresh_name "argument" in
        let code =
          code ^ generated.code
          ^ line level
              (string_of_c_value_type expected_type ^ " " ^ value_name ^ " = "
               ^ generated.value ^ ";")
        in
        evaluate code (value_name :: values) parameters arguments
    | _ -> assert false
  in
  let code, values = evaluate "" [] fn.parameters arguments in
  { code;
    value = fn.c_name ^ "(" ^ String.concat ", " values ^ ")";
    typ = fn.result_type }

and emit_while context condition body level =
  let condition = emit context condition (level + 1) in
  let body = emit context body (level + 1) in
  let code =
    line level "while (true) {"
    ^ condition.code
    ^ line (level + 1) ("if (!(" ^ condition.value ^ ")) break;")
    ^ discard (level + 1) body
    ^ line level "}"
  in
  { code; value = "0"; typ = TUnit }

and emit_for context name first last body level =
  let first = emit context first level in
  let first_code, first_value = materialize level "for_first" first in
  let last = emit context last level in
  let last_code, last_value = materialize level "for_last" last in
  let loop_name = fresh_name (sanitize_identifier name) in
  let loop_local = { local_source = name; local_c_name = loop_name; local_type = TInt } in
  let body_context = { context with locals = loop_local :: context.locals } in
  let body = emit body_context body (level + 1) in
  let code =
    first_code ^ last_code
    ^ line level ("if (" ^ first_value ^ " <= " ^ last_value ^ ") {")
    ^ line (level + 1) ("int " ^ loop_name ^ " = " ^ first_value ^ ";")
    ^ line (level + 1) "for (;;) {"
    ^ discard (level + 2) body
    ^ line (level + 2) ("if (" ^ loop_name ^ " == " ^ last_value ^ ") break;")
    ^ line (level + 2) ("++" ^ loop_name ^ ";")
    ^ line (level + 1) "}"
    ^ line level "}"
  in
  { code; value = "0"; typ = TUnit }

and emit_match context matched cases level =
  let result_type = infer context (Match (matched, cases)) in
  let matched = emit context matched level in
  let matched_code, matched_value = materialize level "match_value" matched in
  let result = fresh_name "match_result" in
  let branches =
    cases
    |> List.mapi (fun index (pattern, branch) ->
         let condition, declarations, pattern_locals =
           emit_pattern pattern matched_value matched.typ (level + 1)
         in
         let branch_context =
           { context with locals = pattern_locals @ context.locals }
         in
         let branch = emit branch_context branch (level + 1) in
         let prefix = if index = 0 then "if" else "else if" in
         line level (prefix ^ " (" ^ condition ^ ") {")
         ^ declarations
         ^ branch.code
         ^ line (level + 1) (result ^ " = " ^ branch.value ^ ";")
         ^ line level "}")
    |> String.concat " "
  in
  let code =
    matched_code
    ^ line level (string_of_c_value_type result_type ^ " " ^ result ^ ";")
    ^ branches ^ " " ^ line level "else {"
    ^ line (level + 1) "ocelot_runtime_match_failure();"
    ^ line level "}"
  in
  { code; value = result; typ = result_type }

and emit_pattern pattern value expected_type level =
  match pattern with
  | PInt integer -> value ^ " == " ^ string_of_int integer, "", []
  | PBool boolean -> value ^ " == " ^ (if boolean then "true" else "false"), "", []
  | PWildcard -> "true", "", []
  | PVar name ->
      let c_name = fresh_name (sanitize_identifier name) in
      let local = { local_source = name; local_c_name = c_name; local_type = expected_type } in
      let declaration =
        line level (string_of_c_value_type expected_type ^ " " ^ c_name ^ " = " ^ value ^ ";")
      in
      "true", declaration, [local]
  | PNil -> value ^ " == NULL", "", []
  | PCons (head, tail) ->
      let item_type =
        match expected_type with
        | TList item -> item
        | typ -> raise (Error ("motif de liste appliqué au type " ^ W.string_of_typ typ))
      in
      let head_condition, head_declarations, head_locals =
        emit_pattern head ("(" ^ value ^ ")->value") item_type level
      in
      let tail_condition, tail_declarations, tail_locals =
        emit_pattern tail ("(" ^ value ^ ")->next") expected_type level
      in
      ("(" ^ value ^ " != NULL && " ^ head_condition ^ " && " ^ tail_condition ^ ")",
       head_declarations ^ tail_declarations,
       head_locals @ tail_locals)

and emit_sequence context expressions level =
  match expressions with
  | [] -> { code = ""; value = "0"; typ = TUnit }
  | [last] -> emit context last level
  | expression :: rest ->
      let generated = emit context expression level in
      let tail = emit_sequence context rest level in
      { tail with code = discard level generated ^ tail.code }

let prototype fn =
  let parameters =
    match fn.parameters with
    | [] -> "void"
    | parameters ->
        parameters
        |> List.map (fun (_, typ) -> string_of_c_value_type typ)
        |> String.concat ", "
  in
  string_of_c_return_type fn.result_type ^ " " ^ fn.c_name ^ "(" ^ parameters ^ ");\n"

let emit_function functions fn =
  let parameters, locals =
    fn.parameters
    |> List.map (fun (name, typ) ->
         let c_name = fresh_name (sanitize_identifier name) in
         (string_of_c_value_type typ ^ " " ^ c_name,
          { local_source = name; local_c_name = c_name; local_type = typ }))
    |> List.split
  in
  let context = { functions; locals } in
  let body = emit context fn.body 1 in
  let body_code =
    if fn.result_type = TUnit then discard 1 body
    else body.code ^ line 1 ("return " ^ body.value ^ ";")
  in
  string_of_c_return_type fn.result_type ^ " " ^ fn.c_name ^ "("
  ^ (if parameters = [] then "void" else String.concat ", " parameters)
  ^ ") {\n" ^ body_code ^ "}\n"

let runtime =
  "#include <stdbool.h>\n\
   #include <stdio.h>\n\
   #include <stdlib.h>\n\n\
   typedef struct int_list {\n\
     int value;\n\
     struct int_list *next;\n\
     struct int_list *allocation_next;\n\
   } int_list;\n\n\
   static int_list *ocelot_runtime_allocations = NULL;\n\n\
   int_list *ocelot_runtime_cons_int(int value, int_list *next) {\n\
     int_list *node = malloc(sizeof *node);\n\
     if (node == NULL) {\n\
       fputs(\"ocelot: mémoire insuffisante\\n\", stderr);\n\
       exit(EXIT_FAILURE);\n\
     }\n\
     node->value = value;\n\
     node->next = next;\n\
     node->allocation_next = ocelot_runtime_allocations;\n\
     ocelot_runtime_allocations = node;\n\
     return node;\n\
   }\n\n\
   void ocelot_runtime_free_all(void) {\n\
     while (ocelot_runtime_allocations != NULL) {\n\
       int_list *next = ocelot_runtime_allocations->allocation_next;\n\
       free(ocelot_runtime_allocations);\n\
       ocelot_runtime_allocations = next;\n\
     }\n\
   }\n\n\
   int ocelot_runtime_div(int left, int right) {\n\
     if (right == 0) {\n\
       fputs(\"ocelot: division par zéro\\n\", stderr);\n\
       exit(EXIT_FAILURE);\n\
     }\n\
     return left / right;\n\
   }\n\n\
   int ocelot_runtime_mod(int left, int right) {\n\
     if (right == 0) {\n\
       fputs(\"ocelot: modulo par zéro\\n\", stderr);\n\
       exit(EXIT_FAILURE);\n\
     }\n\
     return left % right;\n\
   }\n\n\
   _Noreturn void ocelot_runtime_match_failure(void) {\n\
     fputs(\"ocelot: filtrage non exhaustif\\n\", stderr);\n\
     exit(EXIT_FAILURE);\n\
   }\n\n"

let translate_program functions main_expression =
  name_counter := 0;
  let functions = List.filter (fun fn -> fn.nom <> "()") functions in
  let infos = infer_toplevel functions main_expression in
  let prototypes = infos |> List.map prototype |> String.concat "" in
  let definitions = infos |> List.map (emit_function infos) |> String.concat "\n" in
  let main_context = { functions = infos; locals = [] } in
  let main = emit main_context main_expression 1 in
  runtime ^ prototypes ^ "\n" ^ definitions
  ^ "\nint main(void) {\n" ^ discard 1 main
  ^ line 1 "ocelot_runtime_free_all();" ^ line 1 "return 0;" ^ "}\n"

let find_main functions =
  match List.filter (fun fn -> fn.nom = "()") functions with
  | [main] -> main.corps
  | [] -> raise (Error "le fichier ne contient pas de point d'entrée `let () = ...`")
  | _ -> raise (Error "le fichier contient plusieurs points d'entrée `let () = ...`")

let chercher_main = find_main

let write_c_file filename functions =
  let main_expression = find_main functions in
  let generated = translate_program functions main_expression in
  let channel = open_out_bin filename in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel generated)
