let fail message =
  prerr_endline ("ÉCHEC : " ^ message);
  exit 1

let check condition message = if not condition then fail message

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec search index =
    if index + fragment_length > text_length then false
    else if String.sub text index fragment_length = fragment then true
    else search (index + 1)
  in
  fragment_length = 0 || search 0

let parse source =
  let lexbuf = Lexing.from_string source in
  Parser.programme Lexer.token lexbuf

let translate source =
  let functions = parse source in
  let main = Translate.find_main functions in
  Translate.translate_program functions main

let expect_type_error source =
  try
    ignore (translate source);
    false
  with W.TypeError _ -> true

let () =
  let arithmetic = translate "let add x y = x + y let () = print_int (add 3 4)" in
  check (contains arithmetic "ocelot_user_add(") "un appel à plusieurs arguments doit devenir un appel C";
  check
    (not (contains arithmetic ")(ocelot_tmp_argument"))
    "un appel curryfié ne doit pas produire f(x)(y)";

  let matching =
    translate
      "let head l = match l with | [] -> 0 | x :: _ -> x \
       let () = print_int (head [9; 8])"
  in
  check (contains matching "!= NULL") "un motif cons doit vérifier la liste avant de la déréférencer";
  check (contains matching "ocelot_runtime_match_failure") "un match non exhaustif doit échouer explicitement";

  let control_flow =
    translate
      "let () = if true && not false then (print_bool true; print_newline ()) else print_bool false"
  in
  check (contains control_flow "bool ocelot_tmp_logical_result") "&& doit conserver le court-circuit";
  check (contains control_flow "putchar('\\n')") "print_newline () doit être pris en charge";

  let mangled = translate "let switch auto = auto + 1 let () = print_int (switch (-1))" in
  check (contains mangled "ocelot_user_switch") "les identifiants doivent être protégés des mots-clés C";

  check
    (expect_type_error "let () = print_int (1 + true)")
    "une addition entre int et bool doit être rejetée";

  print_endline "Tous les tests OCaml sont passés."
