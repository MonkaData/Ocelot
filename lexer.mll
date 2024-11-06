{
(*faudrait open Parser mais y'en a pas encore : ( *) 

(* Exception pour les caractères inconnus *)
exception Unknown_character of char
}

rule read = parse
  | [' ' '\t' '\n'] { read lexbuf }  (*https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html*)
  | ['0'-'9']+ as num { (* Explication rapide :  [0-9] intervalle des caractères en ASCII compris entre ces deux bornes *)

      let value = int_of_string num in
      INT(value) (* Entrée : string, on stock la valeur pour ensuite la manipuler*)
    }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIVIDE }
  | eof { EOF }
  | _  { raise (Unknown_character) (* cf l.6 *)
}
