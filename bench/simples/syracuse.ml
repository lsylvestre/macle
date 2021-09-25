circuit s(n) = 
  let rec odd(n) = 
    if n = 0 then false else even(n-1) 
  and even(n) = 
    if n = 0 then true else odd(n-1) 
  in
  let rec syracuse(t,n) =
    if n = 1 then t else
    if even(n) then syracuse(t+1,n/2) 
    else syracuse(t+1,3*n+1)
  in
  syracuse(0,n) 
 

;;;;
