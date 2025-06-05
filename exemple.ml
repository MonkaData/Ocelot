let test1 x = if x = 1 then true else false

let test2 x = if test1 x then 0 else 1

let () = 
  let j = 5 in 
  for i = 0 to j do 
    print_int (test1 i)
  done; 
  while true do 
    print_int (test2 (test2 1))
  done
