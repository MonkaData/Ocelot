{
(*faudrait open Parser mais y'en a pas encore : ( *) 

(* Exception pour les caractères inconnus *)
exception Unknown_character of char
}

rule read = parse
  | [' ' '\t' '\n'] { read suivant }  (*On ignore les espaces récursivement *)
  | ['0'-'9']+ as num { (* Explication rapide :  [0-9] intervalle des caractères en ASCII compris entre ces deux bornes, + <=> * en langage ( séquence >= 1 chiffre ) *)

      let value = int_of_string num in
      INT(value) (* Entrée : string, on stock la valeur pour ensuite la manipuler*)
    }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIVIDE }
  | eof { EOF }
  | _ as char { raise (Unknown_character char) (* cf l.6 *)
}
