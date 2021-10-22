circuit fact(m) = 
  let rec accumulate(f,n,a) = 
    if n <= 0 then a 
    else accumulate(f,n-1,f(n,a)) 
  in
  let mult(a,b) = a * b in
  accumulate(mult,m,1)
;;;;

