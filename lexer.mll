{
  open Parser
  open Lexing

  exception Error of string

  let position lexbuf =
    let pos = lexbuf.lex_curr_p in
    Printf.sprintf "ligne %d, colonne %d"
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

  let keywords = Hashtbl.create 23

  let () =
    List.iter (fun (word, token) -> Hashtbl.add keywords word token)
      [ "let", LET; "rec", REC; "and", AND; "in", IN;
        "if", IF; "then", THEN; "else", ELSE;
        "while", WHILE; "for", FOR; "do", DO; "done", DONE; "to", TO;
        "fun", FUNCTION; "function", FUNCTION; "not", NOT;
        "true", TRUE; "false", FALSE; "match", MATCH; "with", WITH ]
}

rule token = parse
  | [' ' '\t' '\r']+              { token lexbuf }
  | '\n'                           { new_line lexbuf; token lexbuf }
  | "(*"                           { comment 1 lexbuf }
  | "->"                           { ARROW }
  | "&&"                           { LAND }
  | "||"                           { OR }
  | "::"                           { CONS }
  | ">="                           { SUPEGAL }
  | "<="                           { INFEGAL }
  | "|"                            { BAR }
  | "+"                            { PLUS }
  | "-"                            { MINUS }
  | "*"                            { TIMES }
  | "/"                            { DIV }
  | "mod"                          { MOD }
  | "["                            { LBRACKET }
  | "]"                            { RBRACKET }
  | ";"                            { SEMI }
  | "()"                           { UNIT }
  | "("                            { LPAREN }
  | ")"                            { RPAREN }
  | ">"                            { SUP }
  | "<"                            { INF }
  | "="                            { EGAL }
  | ['0'-'9']+ as value            {
      try ENTIER (int_of_string value)
      with Failure _ -> raise (Error ("entier hors limites à " ^ position lexbuf))
    }
  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']* as id {
      match Hashtbl.find_opt keywords id with
      | Some keyword -> keyword
      | None -> IDENT id
    }
  | eof                            { EOF }
  | _ as c                         {
      raise (Error (Printf.sprintf "caractère inattendu %C à %s" c (position lexbuf)))
    }

and comment depth = parse
  | "(*"                           { comment (depth + 1) lexbuf }
  | "*)"                           { if depth = 1 then token lexbuf else comment (depth - 1) lexbuf }
  | '\n'                           { new_line lexbuf; comment depth lexbuf }
  | eof                            { raise (Error ("commentaire non terminé à " ^ position lexbuf)) }
  | _                              { comment depth lexbuf }
