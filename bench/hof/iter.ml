circuit main(m) = 
  let rec iter(f,n,a) = 
    if n <= 0 then a else iter(f,n-1,f(n,a)) in
  let plus(a,b) = a + b in
  iter(plus,m,0)
;;;;

