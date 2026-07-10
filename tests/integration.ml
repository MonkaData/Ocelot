let add x y = x + y

let head l =
  match l with
  | [] -> 0
  | x :: _ -> x

let () =
  print_int (add 3 4);
  print_newline ();
  print_bool (not false && true);
  print_newline ();
  for i = 1 to 3 do
    print_int i
  done;
  print_newline ();
  print_int (head [9; 8]);
  print_newline ()
