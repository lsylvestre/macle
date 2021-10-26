circuit main(n) = 
  let rec g(f,m) = let x = f(2,3) in g(f,m) in
  let plus(a,b) = a + b in
  g(plus,n)
;;;;

