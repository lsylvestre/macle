circuit f(a,b,c) = 
  let rec fact(n) =
    let rec aux(acc,n) = 
      if n <= 0 then acc else aux(acc*n,n-1) in
      aux(1,n) 
  in
  fact(a) + fact(b) + fact(c)

;;;

print_int @@ f 5 6 7;;