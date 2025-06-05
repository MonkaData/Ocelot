let rec length l =
  match l with
  [] -> 0
  | _ :: t -> 1 + length t

let rec bubble_once l =
  match l with
  [] -> []
  | x :: [] -> x :: []
  | x :: y :: rest ->
      if x > y then y :: bubble_once (x :: rest)
      else x :: bubble_once (y :: rest)

let rec bubble_aux l n =
  match n with
  0 -> l
  | _ -> bubble_aux (bubble_once l) (n - 1)

let bubble_sort l =
  bubble_aux l (length l)

let print_head l =
  match l with
  [] -> ()
  | x :: _ -> print_int x

let () =
  print_head (bubble_sort [5; 3; 1; 4; 2])
