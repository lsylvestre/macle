circuit ci(n) = 
  let rec sum(n) =
    let rec aux(acc,n) = 
      if n <= 0 then acc else aux(acc+n,n-1) in
      aux(0,n) 
  in
  let rec repeat(acc,i) =
    if i >= n then acc else 
    repeat(acc+sum(i),i+1)
  in
  repeat(0,0)

;;;

print_int @@ ci 10 ;;