circuit f(a,b,c,d) = 
  let rec fact(n) =
    let rec aux(acc,n) = 
      if n <= 0 then acc else aux(acc*n,n-1) in
      aux(1,n) 
  in
  fact(a) + fact(b) + fact(c) + fact(d)

;;;

print_int @@ f 5 6 7 8;;