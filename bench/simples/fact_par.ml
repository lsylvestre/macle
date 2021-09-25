circuit fact(a,b) = 
  let rec fact(n) =
    let rec aux(acc,n) = 
      if n <= 0 then acc else aux(acc*n,n-1) in
      aux(1,n) 
  in
  let rec main() =
    let x = fact(a)
    and y = fact(b)
    in
     x + y
  in 
  main()

;;;