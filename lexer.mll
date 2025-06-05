{
  open Parser
  open Lexing

  (* Fonction utilitaire pour afficher la position dans le fichier *)
  let print_position lexbuf =
    let pos = lexbuf.lex_curr_p in
    Printf.sprintf "ligne %d, colonne %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

  (* Table de hachage pour distinguer les mots-clés des identifiants généraux *)
  let keyword_table = Hashtbl.create 17

  let () =
    List.iter (fun (kw, tok) ->
      Hashtbl.add keyword_table kw tok)
    [ ("let", LET);
      ("in", IN);
      ("if", IF);
      ("then", THEN);
      ("else", ELSE);
      ("while", WHILE);
      ("for", FOR);
      ("do", DO);
      ("to", TO);
      ("function", FUNCTION);
      ("not", NOT);
      ("true", TRUE);
      ("false", FALSE);
      ("rec", REC); (* Added rec to keyword table *)
      ("and", AND);  (* Added and to keyword table, maps to the same AND token as && *)
      ("match", MATCH);
      ("with", WITH)
    ]
}

rule token = parse
  | [' ' '\t' '\r' '\n']+         { token lexbuf }
  (* "rec" is now handled by the keyword_table *)
  | "(*"                          { comment 1 lexbuf }
  | "->"                          { ARROW }
  | "&&"                          { AND }
  | "||"                          { OR }
  | "|"                           { BAR }
  | "+"                           { PLUS }
  | "-"                           { MINUS }
  | "*"                           { TIMES }
  | "/"                           { DIV }
  | "mod"                         { MOD }
  | "::"                          { CONS }
  | "["                           { LBRACKET }
  | "]"                           { RBRACKET }
  | ";"                           { SEMI }
  | "()"                          { IDENT "()" }
  | "done"                        { token lexbuf }  (* Ignorer le mot-clé "done" *)
  | "("                           { LPAREN }
  | ")"                           { RPAREN }
  | ">="                          { SUPEGAL }
  | "<="                          { INFEGAL }
  | ">"                           { SUP }
  | "<"                           { INF }
  | "="                           { EGAL }
  | ['0'-'9']+ as num             { ENTIER (int_of_string num) }
  | ['a'-'z' 'A'-'Z' '_']
    ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id
                                  { 
                                    try Hashtbl.find keyword_table id
                                    with Not_found -> IDENT id
                                  }
  | eof                           { EOF }
  | _ as c                        { failwith ("Erreur lexicale à " ^ (print_position lexbuf) ^
                                        " : caractère inattendu " ^ (String.make 1 c)) }

and comment depth = parse
  | "(*"                          { comment (depth + 1) lexbuf }
  | "*)"                          {
                                    if depth = 1 then token lexbuf
                                    else comment (depth - 1) lexbuf
                                  }
  | eof                           { failwith ("Commentaire non terminé à " ^ (print_position lexbuf)) }
  | _                             { comment depth lexbuf }
