let output_file = ref None
let input_file = ref None

let usage = "Usage : ocelot [-o fichier.c] fichier.ml"

let set_input filename =
  match !input_file with
  | None -> input_file := Some filename
  | Some _ -> raise (Arg.Bad "un seul fichier source peut être transpilé")

let options =
  [ "-o", Arg.String (fun filename -> output_file := Some filename),
    "fichier.c  Choisir le fichier C généré";
    "--output", Arg.String (fun filename -> output_file := Some filename),
    "fichier.c  Alias de -o";
    "--version", Arg.Unit (fun () -> print_endline "Ocelot 0.2.0"; exit 0),
    "           Afficher la version" ]

let default_output filename =
  try Filename.chop_extension filename ^ ".c"
  with Invalid_argument _ -> filename ^ ".c"

let absolute_path filename =
  let path =
    if Filename.is_relative filename then Filename.concat (Sys.getcwd ()) filename
    else filename
  in
  String.lowercase_ascii path

let parse filename =
  let channel = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let lexbuf = Lexing.from_channel channel in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      try Parser.programme Lexer.token lexbuf with
      | Parser.Error ->
          let position = lexbuf.lex_curr_p in
          let column = position.pos_cnum - position.pos_bol + 1 in
          raise
            (Failure
               (Printf.sprintf "%s:%d:%d: erreur de syntaxe"
                  filename position.pos_lnum column)))

let run () =
  Arg.parse options set_input usage;
  let input =
    match !input_file with
    | Some filename -> filename
    | None -> raise (Arg.Bad "aucun fichier source fourni")
  in
  let output = Option.value !output_file ~default:(default_output input) in
  if absolute_path input = absolute_path output then
    raise (Arg.Bad "le fichier de sortie doit être différent du fichier source");
  let functions = parse input in
  Translate.write_c_file output functions;
  Printf.printf "Fichier C généré : %s\n" output

let () =
  try run () with
  | Arg.Bad message ->
      Printf.eprintf "%s\n%s\n" message usage;
      exit 2
  | Sys_error message ->
      Printf.eprintf "Erreur d'entrée/sortie : %s\n" message;
      exit 1
  | Lexer.Error message ->
      Printf.eprintf "Erreur lexicale : %s\n" message;
      exit 1
  | W.TypeError message ->
      Printf.eprintf "Erreur de typage : %s\n" message;
      exit 1
  | Translate.Error message ->
      Printf.eprintf "Erreur de traduction : %s\n" message;
      exit 1
  | Failure message ->
      Printf.eprintf "%s\n" message;
      exit 1
