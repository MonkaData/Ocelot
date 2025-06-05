(* w.ml *)

open Ast

exception TypeError of string

(* Fonctions auxiliaires sur les listes : union de listes sans doublons *)
let union l1 l2 =
  List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) l1 l2

(* Les types du langage *)
type typ =
  | TInt
  | TBool
  | TUnit
  | TFun of typ * typ
  | TVar of int

(* Facilite l'affichage *)
let rec type_en_str t =
  match t with
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TFun(t1, t2) -> "fun (" ^ (type_en_str t1) ^ ") (" ^ (type_en_str t2) ^ ")"
  | TVar(i) -> "var : " ^ (string_of_int i)

(* Un schéma de type, permettant la généralisation (let-polymorphisme) *)
type scheme = Forall of int list * typ

(* L'environnement de typage associe un identificateur à un schéma *)
type env = (ident * scheme) list

(* Une substitution est une liste d'associations d'une variable de type à un type *)
type substitution = (int * typ) list

(* Génération de nouvelles variables et identification *)
let counter = ref 0
let fresh_tyvar () =
  let n = !counter in
  counter := n + 1;
  TVar n

(* Application d'une substitution à un type *)
let rec apply_subst (subst : substitution) (t : typ) : typ =
  match t with
  | TInt | TBool | TUnit -> t
  | TFun(t1, t2) -> TFun(apply_subst subst t1, apply_subst subst t2)
  | TVar n ->
    (try
       let t' = List.assoc n subst in
       apply_subst subst t'
     with Not_found -> TVar n)

(* Composition de substitutions : (s1 o s2) applique d'abord s2 puis s1 *)
let compose_subst s1 s2 =
  let s2' = List.map (fun (n, t) -> (n, apply_subst s1 t)) s2 in
  s2' @ s1

(* Calcul des variables libres *)
let rec ftv (t : typ) : int list =
  match t with
  | TInt | TBool | TUnit -> []
  | TFun(t1, t2) -> union (ftv t1) (ftv t2)
  | TVar n -> [n]

let ftv_scheme (Forall(vars, t)) =
  List.filter (fun n -> not (List.mem n vars)) (ftv t)

let ftv_env (env : env) =
  List.fold_left (fun acc (_, sch) -> union acc (ftv_scheme sch)) [] env


(* Généralisation et instanciation *)
let generalize (env : env) (t : typ) : scheme =
  let env_ftv = ftv_env env in
  let vars = List.filter (fun n -> not (List.mem n env_ftv)) (ftv t) in
  Forall(vars, t)

let instantiate (Forall(vars, t) : scheme) : typ =
  let subst = List.map (fun n -> (n, fresh_tyvar ())) vars in
  apply_subst subst t


(* Unification *)
let rec unify t1 t2 : substitution =
  match t1, t2 with
  | TInt, TInt | TBool, TBool | TUnit, TUnit -> []
  | TFun(l1, r1), TFun(l2, r2) ->
    let s0 = unify l1 l2 in
    let s1 = unify (apply_subst s0 r1) (apply_subst s0 r2) in
    compose_subst s1 s0
  | TVar n, t | t, TVar n ->
    if t = TVar n then [] else
    if List.mem n (ftv t) then raise (TypeError "Échec de l'occurs check")
    else [(n, t)]
  | _ -> raise (TypeError ("Types incompatibles lors de l'unification: " ^
                             (type_en_str t1) ^ " et " ^ (type_en_str t2)))


(* Application d'une substitution à l'environnement *)
let apply_subst_env (subst : substitution) (env : env) : env =
  List.map (fun (x, Forall(vars, t)) ->
      (x, Forall(vars, apply_subst subst t))
    ) env


(* Algorithme W *)
let rec w (env : env) (e : expr) : substitution * typ =
  match e with
  | Entier _ -> ([], TInt)
  | Booleen _ -> ([], TBool)
  | Variable x ->
      if x = "print_int" then ([], TFun(TInt, TUnit))
      else
        (try
           let sch = List.assoc x env in
           ([], instantiate sch)
         with Not_found ->
           raise (TypeError ("Variable non déclarée: " ^ x)))
  | Binop (op, e1, e2) ->
    begin match op with
    | Plus | Moins | Fois | Divise | Modulo ->
      let s0, t1 = w env e1 in
      let s1, t2 = w env e2 in
      let s_temp = compose_subst s1 s0 in
      let s3 = unify (apply_subst s_temp t1) TInt in
      let s4 = compose_subst s3 s_temp in
      let s_temp2 = unify (apply_subst s4 t2) TInt in
      let s_final = compose_subst s_temp2 s4 in
      (s_final, TInt)
    | Et | Ou ->
      let s0, t1 = w env e1 in
      let s1, t2 = w env e2 in
      let s_temp = compose_subst s1 s0 in
      let s3 = unify (apply_subst s_temp t1) TBool in
      let s4 = compose_subst s3 s_temp in
      let s_temp2 = unify (apply_subst s4 t2) TBool in
      let s_final = compose_subst s_temp2 s4 in
      (s_final, TBool)
    | Inf | Sup | Infegal | Supegal | Egal ->
      let s0, t1 = w env e1 in
      let s1, t2 = w env e2 in
      let s_temp = compose_subst s1 s0 in
      let s3 = unify (apply_subst s_temp t1) TInt in
      let s4 = compose_subst s3 s_temp in
      let s_temp2 = unify (apply_subst s4 t2) TInt in
      let s_final = compose_subst s_temp2 s4 in
      (s_final, TBool)
    end
  | Unop (Non, e) ->
    let s, t = w env e in
    let s' = unify t TBool in
    (compose_subst s' s, TBool)
  | Si (cond, e_then, opt_e_else) ->
    let s0, t_cond = w env cond in
    let s1 = unify t_cond TBool in
    let s2 = compose_subst s1 s0 in
    let s3, t_then = w env e_then in
    let s4 = compose_subst s3 s2 in
    (match opt_e_else with
     | Some e_else ->
       let s5, t_else = w env e_else in
       let s6 = compose_subst s5 s4 in
       let s7 = unify (apply_subst s6 t_then) (apply_subst s6 t_else) in
       let s_final = compose_subst s7 s6 in
       (s_final, apply_subst s_final t_then)
     | None ->
       let s5 = unify (apply_subst s4 t_then) TUnit in
       let s_final = compose_subst s5 s4 in
       (s_final, TUnit))
  | Let (x, e1, e2) ->
    let s0, t1 = w env e1 in
    let env' = apply_subst_env s0 env in
    let sch = generalize env' t1 in
    let env'' = (x, sch) :: env' in
    let s1, t2 = w env'' e2 in
    (compose_subst s1 s0, t2)
  | Fonction (x, body) ->
    let tv = fresh_tyvar () in
    let env' = (x, Forall ([], tv)) :: env in
    let s, t_body = w env' body in
    (s, TFun(apply_subst s tv, t_body))
  | Application (e1, e2) ->
    let s0, t1 = w env e1 in
    let env' = apply_subst_env s0 env in
    let s1, t2 = w env' e2 in
    let s2 = compose_subst s1 s0 in
    let tv = fresh_tyvar () in
    let s3 = unify (apply_subst s2 t1) (TFun(t2, tv)) in
    let s_final = compose_subst s3 s2 in
    (s_final, apply_subst s_final tv)
  | TantQue (cond, body) ->
    let s0, t_cond = w env cond in
    let s1 = unify t_cond TBool in
    let s2 = compose_subst s1 s0 in
    let s3, t_body = w env body in
    let s4 = unify t_body TUnit in
    let s_final = compose_subst s4 s3 in
    (s_final, TUnit)
  | Pour (x, e1, e2, body) ->
    let s0, t1 = w env e1 in
    let s1 = unify t1 TInt in
    let s2 = compose_subst s1 s0 in
    let s3, t2 = w env e2 in
    let s4 = unify t2 TInt in
    let s5 = compose_subst s4 s3 in
    let env' = (x, Forall ([], TInt)) :: env in
    let s6, t_body = w env' body in
    let s7 = unify t_body TUnit in
    let s_final = compose_subst s7 s6 in
    (s_final, TUnit)
  | Sequence exprs ->
    let rec infer_sequence env exprs =
      match exprs with
      | [] -> ([], TUnit)
      | [e] -> w env e
      | e :: es ->
        let s0, _ = w env e in
        let env' = apply_subst_env s0 env in
        let s1, t_last = infer_sequence env' es in
        (compose_subst s1 s0, t_last)
    in
    infer_sequence env exprs

(* Fonctions d'affichage (pour le débogage) *)
let rec string_of_typ t =
  match t with
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TFun(t1, t2) -> "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
  | TVar n -> "'a" ^ string_of_int n

let infer_expr env e =
  let s, t = w env e in
  apply_subst s t

(* Gestion des définitions globales *)
let rec w_global (env : env) (defs : (ident * expr) list) : env =
  match defs with
  | [] -> env
  | (id, e) :: rest ->
      let s, t = w env e in
      let t = apply_subst s t in
      let sch = generalize env t in
      let env' = (id, sch) :: env in
      w_global env' rest

let infer_program (defs : (ident * expr) list) (main_expr : expr) : typ =
  let global_env = w_global [] defs in
  let s, t = w global_env main_expr in
  apply_subst s t
