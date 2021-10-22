circuit ci(a,b,c,d,e,f) = 
  let rec fact(n) =
    let rec aux(acc,n) = 
      if n <= 0 then acc else aux(acc*n,n-1) in
      aux(1,n) 
  in
  fact(a)

;;;

print_int @@ ci 4 5 6 7 8 9;;