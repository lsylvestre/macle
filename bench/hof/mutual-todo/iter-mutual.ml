circuit main(m) = 
  let rec iter(f,n,a) =
    if n <= 0 then a else iter2(f,n-1,f(n,a))
  and iter2(f1,n1,a1) =
    if n1 <= 0 then a1 else iter(f1,n1-2,f1(n1,a1)) 
  in
  let plus(a,b) = a + b in
  iter(plus,m,0)
;;;;

