(* main.ml *)

open Lexing

let () =
  Printexc.record_backtrace true; (* Permet de suivre l'arbre d'exceptions levé *)
  if Array.length Sys.argv < 2 then ( 
    Printf.eprintf "Usage : %s <fichier_input.ml>\n" Sys.argv.(0);
    exit 1
  );
  let input_file = Sys.argv.(1) in (* Lecture de la commande terminal *)
  let ic = open_in input_file in
  let lexbuf = Lexing.from_channel ic in
  try
    (* Analyse lexicale et syntaxique pour obtenir l'AST (liste de fonctions) *)
    let functions = Parser.programme Lexer.token lexbuf in
    close_in ic;
    (* Traduction vers C en écrivant dans output.c *)
    Translate.write_c_file "output.c" functions;
    Printf.printf "Traduction terminée. Consultez output.c.\n"
  with
  | Parser.Error -> (*Erreur de parsing*)
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "Erreur de syntaxe au caractère %d\n"
        (pos.pos_cnum - pos.pos_bol + 1);
      exit 1
  | W.TypeError msg -> (* Erreur de typage *)
      Printf.eprintf "Erreur de typage : %s\n" msg;
      exit 1
  | e -> (* Autres erreur pouvant survenir (le plus souvent lors de la traduction dans translate.ml : plusieurs failwith y sont déclaré) *)
      Printf.eprintf "Erreur inattendue : %s\n" (Printexc.to_string e);
      Printexc.print_backtrace stderr;
      exit 1