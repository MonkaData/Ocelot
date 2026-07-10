open Ast

exception TypeError of string

type typ =
  | TInt
  | TBool
  | TUnit
  | TList of typ
  | TFun of typ * typ
  | TVar of int

type scheme = Forall of int list * typ
type env = (ident * scheme) list
type substitution = (int * typ) list

let counter = ref 0
let fresh_tyvar () =
  let id = !counter in
  incr counter;
  TVar id

let reset_fresh_vars () = counter := 0

let union left right =
  List.fold_left
    (fun result item -> if List.mem item result then result else item :: result)
    left right

let rec string_of_typ = function
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TList item -> string_of_typ item ^ " list"
  | TFun (argument, result) ->
      "(" ^ string_of_typ argument ^ " -> " ^ string_of_typ result ^ ")"
  | TVar id -> "'a" ^ string_of_int id

let type_en_str = string_of_typ

let rec ftv = function
  | TInt | TBool | TUnit -> []
  | TList item -> ftv item
  | TFun (argument, result) -> union (ftv argument) (ftv result)
  | TVar id -> [id]

let rec apply_subst subst typ =
  match typ with
  | TInt | TBool | TUnit -> typ
  | TList item -> TList (apply_subst subst item)
  | TFun (argument, result) ->
      TFun (apply_subst subst argument, apply_subst subst result)
  | TVar id ->
      (match List.assoc_opt id subst with
       | None -> typ
       | Some replacement -> apply_subst subst replacement)

let compose_subst newer older =
  let older = List.map (fun (id, typ) -> id, apply_subst newer typ) older in
  older @ newer

let ftv_scheme (Forall (bound, typ)) =
  List.filter (fun id -> not (List.mem id bound)) (ftv typ)

let ftv_env env =
  List.fold_left (fun result (_, scheme) -> union result (ftv_scheme scheme)) [] env

let apply_subst_scheme subst (Forall (bound, typ)) =
  let subst = List.filter (fun (id, _) -> not (List.mem id bound)) subst in
  Forall (bound, apply_subst subst typ)

let apply_subst_env subst env =
  List.map (fun (name, scheme) -> name, apply_subst_scheme subst scheme) env

let generalize env typ =
  let env_variables = ftv_env env in
  let variables = List.filter (fun id -> not (List.mem id env_variables)) (ftv typ) in
  Forall (variables, typ)

let instantiate (Forall (variables, typ)) =
  let subst = List.map (fun id -> id, fresh_tyvar ()) variables in
  apply_subst subst typ

let rec unify left right =
  match left, right with
  | TInt, TInt | TBool, TBool | TUnit, TUnit -> []
  | TList left, TList right -> unify left right
  | TFun (left_arg, left_result), TFun (right_arg, right_result) ->
      let first = unify left_arg right_arg in
      let second =
        unify (apply_subst first left_result) (apply_subst first right_result)
      in
      compose_subst second first
  | TVar id, typ | typ, TVar id ->
      if typ = TVar id then []
      else if List.mem id (ftv typ) then
        raise (TypeError ("type récursif impossible pour " ^ string_of_typ (TVar id)))
      else [id, typ]
  | _ ->
      raise
        (TypeError
           ("types incompatibles : " ^ string_of_typ left ^ " et " ^ string_of_typ right))

let lookup env name =
  match List.assoc_opt name env with
  | Some scheme -> instantiate scheme
  | None -> raise (TypeError ("variable non déclarée : " ^ name))

let merge_pattern_env left right =
  List.fold_left
    (fun result (name, scheme) ->
      if List.mem_assoc name result then
        raise (TypeError ("variable liée plusieurs fois dans un motif : " ^ name));
      (name, scheme) :: result)
    left right

let rec infer_pattern pattern =
  match pattern with
  | PInt _ -> [], TInt, []
  | PBool _ -> [], TBool, []
  | PVar name ->
      let typ = fresh_tyvar () in
      [], typ, [name, Forall ([], typ)]
  | PWildcard -> [], fresh_tyvar (), []
  | PNil -> [], TList (fresh_tyvar ()), []
  | PCons (head, tail) ->
      let head_subst, head_type, head_env = infer_pattern head in
      let tail_subst, tail_type, tail_env = infer_pattern tail in
      let combined = compose_subst tail_subst head_subst in
      let item_type = fresh_tyvar () in
      let list_subst = unify (apply_subst combined tail_type) (TList item_type) in
      let combined = compose_subst list_subst combined in
      let head_subst =
        unify (apply_subst combined head_type) (apply_subst combined item_type)
      in
      let subst = compose_subst head_subst combined in
      let env =
        merge_pattern_env
          (apply_subst_env subst head_env)
          (apply_subst_env subst tail_env)
      in
      subst, TList (apply_subst subst item_type), env

let builtin_type = function
  | "print_int" -> Some (TFun (TInt, TUnit))
  | "print_bool" -> Some (TFun (TBool, TUnit))
  | "print_newline" -> Some (TFun (TUnit, TUnit))
  | _ -> None

let rec w env expression =
  match expression with
  | Entier _ -> [], TInt
  | Booleen _ -> [], TBool
  | Unite -> [], TUnit
  | Variable name ->
      (match builtin_type name with
       | Some typ -> [], typ
       | None -> [], lookup env name)
  | Liste expressions -> infer_list env expressions
  | Cons (head, tail) ->
      let head_subst, head_type = w env head in
      let env = apply_subst_env head_subst env in
      let tail_subst, tail_type = w env tail in
      let subst = compose_subst tail_subst head_subst in
      let item_type = fresh_tyvar () in
      let list_subst = unify (apply_subst subst tail_type) (TList item_type) in
      let subst = compose_subst list_subst subst in
      let head_unify =
        unify (apply_subst subst head_type) (apply_subst subst item_type)
      in
      let subst = compose_subst head_unify subst in
      subst, TList (apply_subst subst item_type)
  | Binop (operator, left, right) -> infer_binop env operator left right
  | Unop (Non, expression) ->
      let subst, typ = w env expression in
      let bool_subst = unify (apply_subst subst typ) TBool in
      let subst = compose_subst bool_subst subst in
      subst, TBool
  | Si (condition, then_branch, else_branch) ->
      let condition_subst, condition_type = w env condition in
      let bool_subst = unify (apply_subst condition_subst condition_type) TBool in
      let subst = compose_subst bool_subst condition_subst in
      let env = apply_subst_env subst env in
      let then_subst, then_type = w env then_branch in
      let subst = compose_subst then_subst subst in
      (match else_branch with
       | None ->
           let unit_subst = unify (apply_subst subst then_type) TUnit in
           let subst = compose_subst unit_subst subst in
           subst, TUnit
       | Some else_branch ->
           let env = apply_subst_env subst env in
           let else_subst, else_type = w env else_branch in
           let subst = compose_subst else_subst subst in
           let branch_subst =
             unify (apply_subst subst then_type) (apply_subst subst else_type)
           in
           let subst = compose_subst branch_subst subst in
           subst, apply_subst subst then_type)
  | Let (recursive, bindings, body) -> infer_let env recursive bindings body
  | Fonction (parameter, body) ->
      let parameter_type = fresh_tyvar () in
      let env = (parameter, Forall ([], parameter_type)) :: env in
      let subst, body_type = w env body in
      subst, TFun (apply_subst subst parameter_type, apply_subst subst body_type)
  | Application (fn, argument) ->
      let fn_subst, fn_type = w env fn in
      let env = apply_subst_env fn_subst env in
      let arg_subst, arg_type = w env argument in
      let subst = compose_subst arg_subst fn_subst in
      let result_type = fresh_tyvar () in
      let call_subst =
        unify
          (apply_subst subst fn_type)
          (TFun (apply_subst subst arg_type, result_type))
      in
      let subst = compose_subst call_subst subst in
      subst, apply_subst subst result_type
  | TantQue (condition, body) ->
      let condition_subst, condition_type = w env condition in
      let bool_subst = unify (apply_subst condition_subst condition_type) TBool in
      let subst = compose_subst bool_subst condition_subst in
      let env = apply_subst_env subst env in
      let body_subst, body_type = w env body in
      let subst = compose_subst body_subst subst in
      let unit_subst = unify (apply_subst subst body_type) TUnit in
      let subst = compose_subst unit_subst subst in
      subst, TUnit
  | Pour (name, first, last, body) ->
      let first_subst, first_type = w env first in
      let first_int = unify (apply_subst first_subst first_type) TInt in
      let subst = compose_subst first_int first_subst in
      let env = apply_subst_env subst env in
      let last_subst, last_type = w env last in
      let subst = compose_subst last_subst subst in
      let last_int = unify (apply_subst subst last_type) TInt in
      let subst = compose_subst last_int subst in
      let env = (name, Forall ([], TInt)) :: apply_subst_env subst env in
      let body_subst, body_type = w env body in
      let subst = compose_subst body_subst subst in
      let body_unit = unify (apply_subst subst body_type) TUnit in
      let subst = compose_subst body_unit subst in
      subst, TUnit
  | Match (matched, cases) -> infer_match env matched cases
  | Sequence expressions -> infer_sequence env expressions

and infer_list env expressions =
  let item_type = fresh_tyvar () in
  let rec loop subst = function
    | [] -> subst, TList (apply_subst subst item_type)
    | expression :: rest ->
        let current_env = apply_subst_env subst env in
        let expression_subst, expression_type = w current_env expression in
        let subst = compose_subst expression_subst subst in
        let item_subst =
          unify (apply_subst subst expression_type) (apply_subst subst item_type)
        in
        loop (compose_subst item_subst subst) rest
  in
  loop [] expressions

and infer_binop env operator left right =
  let left_subst, left_type = w env left in
  let env = apply_subst_env left_subst env in
  let right_subst, right_type = w env right in
  let subst = compose_subst right_subst left_subst in
  let require typ =
    let left_required = unify (apply_subst subst left_type) typ in
    let subst = compose_subst left_required subst in
    let right_required = unify (apply_subst subst right_type) typ in
    compose_subst right_required subst
  in
  match operator with
  | Plus | Moins | Fois | Divise | Modulo -> require TInt, TInt
  | Et | Ou -> require TBool, TBool
  | Inf | Sup | Infegal | Supegal -> require TInt, TBool
  | Egal ->
      let equal_subst =
        unify (apply_subst subst left_type) (apply_subst subst right_type)
      in
      let subst = compose_subst equal_subst subst in
      (match apply_subst subst left_type with
       | TInt | TBool -> subst, TBool
       | typ ->
           raise
             (TypeError
                ("l'égalité n'est prise en charge que pour int et bool, pas "
                 ^ string_of_typ typ)))

and infer_let env recursive bindings body =
  if recursive then infer_recursive_let env bindings body
  else
    let rec infer_bindings subst inferred = function
      | [] -> subst, List.rev inferred
      | (name, expression) :: rest ->
          let current_env = apply_subst_env subst env in
          let expression_subst, expression_type = w current_env expression in
          let subst = compose_subst expression_subst subst in
          infer_bindings subst ((name, apply_subst subst expression_type) :: inferred) rest
    in
    let subst, inferred = infer_bindings [] [] bindings in
    let base_env = apply_subst_env subst env in
    let bound_env =
      List.fold_left
        (fun result (name, typ) ->
          (name, generalize base_env (apply_subst subst typ)) :: result)
        base_env inferred
    in
    let body_subst, body_type = w bound_env body in
    let subst = compose_subst body_subst subst in
    subst, apply_subst subst body_type

and infer_recursive_let env bindings body =
  let placeholders = List.map (fun (name, _) -> name, fresh_tyvar ()) bindings in
  let recursive_env =
    List.fold_left
      (fun result (name, typ) -> (name, Forall ([], typ)) :: result)
      env placeholders
  in
  let rec infer subst = function
    | [] -> subst
    | (name, expression) :: rest ->
        let current_env = apply_subst_env subst recursive_env in
        let expression_subst, expression_type = w current_env expression in
        let subst = compose_subst expression_subst subst in
        let expected = List.assoc name placeholders |> apply_subst subst in
        let binding_subst = unify expected (apply_subst subst expression_type) in
        infer (compose_subst binding_subst subst) rest
  in
  let subst = infer [] bindings in
  let base_env = apply_subst_env subst env in
  let bound_env =
    List.fold_left
      (fun result (name, typ) ->
        let typ = apply_subst subst typ in
        (name, generalize base_env typ) :: result)
      base_env placeholders
  in
  let body_subst, body_type = w bound_env body in
  let subst = compose_subst body_subst subst in
  subst, apply_subst subst body_type

and infer_match env matched cases =
  let matched_subst, matched_type = w env matched in
  let result_type = fresh_tyvar () in
  let rec infer_cases subst = function
    | [] -> subst, apply_subst subst result_type
    | (pattern, branch) :: rest ->
        let pattern_subst, pattern_type, pattern_env = infer_pattern pattern in
        let subst = compose_subst pattern_subst subst in
        let pattern_unify =
          unify (apply_subst subst matched_type) (apply_subst subst pattern_type)
        in
        let subst = compose_subst pattern_unify subst in
        let branch_env =
          apply_subst_env subst pattern_env @ apply_subst_env subst env
        in
        let branch_subst, branch_type = w branch_env branch in
        let subst = compose_subst branch_subst subst in
        let result_unify =
          unify (apply_subst subst result_type) (apply_subst subst branch_type)
        in
        infer_cases (compose_subst result_unify subst) rest
  in
  infer_cases matched_subst cases

and infer_sequence env expressions =
  let rec loop subst = function
    | [] -> subst, TUnit
    | [last] ->
        let last_subst, last_type = w (apply_subst_env subst env) last in
        let subst = compose_subst last_subst subst in
        subst, apply_subst subst last_type
    | expression :: rest ->
        let expression_subst, _ = w (apply_subst_env subst env) expression in
        loop (compose_subst expression_subst subst) rest
  in
  loop [] expressions

let infer_expr env expression =
  let subst, typ = w env expression in
  apply_subst subst typ

let rec w_global env definitions =
  match definitions with
  | [] -> env
  | (name, expression) :: rest ->
      let subst, typ = w env expression in
      let env = apply_subst_env subst env in
      let scheme = generalize env (apply_subst subst typ) in
      w_global ((name, scheme) :: env) rest

let infer_program definitions main_expression =
  let global_env = w_global [] definitions in
  infer_expr global_env main_expression
