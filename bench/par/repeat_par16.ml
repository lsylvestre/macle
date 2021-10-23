circuit ci(n) = 
  let rec sum(n) =
    let rec aux(acc,n) = 
      if n <= 0 then acc else aux(acc+n,n-1) in
      aux(0,n) 
  in
  let rec repeat(acc,i) =
    if i >= n then acc else 
    if i >= n-16 then repeat(acc+sum(i),i+1) else 
    let x = sum(i) + sum(i+1) + sum(i+2) + sum(i+3) +
            sum(i+4) + sum(i+5) + sum(i+6) + sum(i+7) +
            sum(i+8) + sum(i+9) + sum(i+10) + sum(i+11) +
            sum(i+12) + sum(i+13) + sum(i+14) + sum(i+15) in
    repeat(acc+x,i+16) 
  in
  repeat(0,0)

;;;

print_int @@ ci 200 ;;