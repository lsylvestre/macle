circuit ci n = 
  let rec sum n =
    let rec aux acc n = 
      if n <= 0 then acc else aux (acc+n) (n-1) in
      aux 0 n
  in
  let rec repeat acc i =
    if i >= n then acc else 
    if i >= n-8 then repeat (acc+sum(i)) (i+1) else 
    let x = sum i + sum (i+1) + sum (i+2) + sum (i+3) +
            sum (i+4) + sum (i+5) + sum (i+6) + sum (i+7) in
    repeat (acc+x) (i+8) 
  in
  repeat 0 0

;;;

print_int @@ ci 200 ;;