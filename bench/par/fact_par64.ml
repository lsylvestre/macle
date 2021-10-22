circuit ci(a,b,c,d) = 
  let rec fact(n) =
    let rec aux(acc,n) = 
      if n <= 0 then acc else aux(acc*n,n-1) in
      aux(1,n) 
  in
  let x = 
    fact(a) + fact(b) + fact(c) + fact(d) +
    fact(a) + fact(b) + fact(c) + fact(d) +
    fact(a) + fact(b) + fact(c) + fact(d) +
    fact(a) + fact(b) + fact(c) + fact(d) +
    fact(a) + fact(b) + fact(c) + fact(d) +
    fact(a) + fact(b) + fact(c) + fact(d) +
    fact(a) + fact(b) + fact(c) + fact(d) +
    fact(a) + fact(b) + fact(c) + fact(d) +
    fact(a) + fact(b) + fact(c) + fact(d) +
    fact(a) + fact(b) + fact(c) + fact(d) +
    fact(a) + fact(b) + fact(c) + fact(d) +
    fact(a) + fact(b) + fact(c) + fact(d) +
    fact(a) + fact(b) + fact(c) + fact(d) +
    fact(a) + fact(b) + fact(c) + fact(d) +
    fact(a) + fact(b) + fact(c) + fact(d) +
    fact(a) + fact(b) + fact(c) + fact(d) 
  in
  x + 1

;;;

print_int @@ ci 5 6 7 8 ;;