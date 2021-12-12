circuit ci a b c d e = 
  let rec fact n =
    let rec aux acc n = 
      if n <= 0 then acc else aux (acc*n) (n-1) in
      aux 1 n 
  in
  let x = fact a + fact b + fact c + fact d + fact e in
  x + 1

;;;

print_int @@ ci 5 6 7 8 9;;