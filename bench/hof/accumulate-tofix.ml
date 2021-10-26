circuit fact(m) = 
  let accumulate(f,n,a) = 
    let rec aux(n,a) =
      if n <= 0 then a 
      else aux(n-1,f(n,a)) 
    in
    let mult(a,b) = a * b in
    aux(n,a) 
  in
  accumulate(mult,m,1)
;;;;

