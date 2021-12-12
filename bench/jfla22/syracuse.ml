circuit sum_syracuse_par1 a b = 
  let stop n =
    let rec syracuse t n =
      if n <= 1 then t else
      if n mod 2=0 then syracuse (t+1) (n/2) 
      else syracuse(t+1) (3*n+1)
    in syracuse 0 n 
  in 
  let rec sum acc i =
    if i >= b then acc else 
    if i > b-1 then sum (acc+stop(i)) (i+1) else
    sum (acc + stop(i)) (i+1) 
  in 
  sum 0 a

;;

circuit sum_syracuse_par2 a b = 
  let stop n =
    let rec syracuse t n =
      if n <= 1 then t else
      if n mod 2=0 then syracuse (t+1) (n/2) 
      else syracuse(t+1) (3*n+1)
    in syracuse 0 n 
  in 
  let rec sum acc i =
    if i >= b then acc else 
    if i > b-2 then sum (acc+stop(i)) (i+1) else
    sum (acc + stop(i) + stop(i+1)) (i+2) 
  in 
  sum 0 a

;;

circuit sum_syracuse_par4 a b = 
  let stop n =
    let rec syracuse t n =
      if n <= 1 then t else
      if n mod 2=0 then syracuse (t+1) (n/2) 
      else syracuse(t+1) (3*n+1)
    in syracuse 0 n 
  in 
  let rec sum acc i =
    if i >= b then acc else 
    if i > b-4 then sum (acc+stop(i)) (i+1) else
    sum (acc + stop(i) + stop(i+1) + stop(i+2) + stop(i+3)) (i+4) 
  in 
  sum 0 a

;;

circuit sum_syracuse_par8 a b = 
  let stop n =
    let rec syracuse t n =
      if n <= 1 then t else
      if n mod 2=0 then syracuse (t+1) (n/2) 
      else syracuse(t+1) (3*n+1)
    in syracuse 0 n 
  in 
  let rec sum acc i =
    if i >= b then acc else 
    if i > b-8 then sum (acc+stop(i)) (i+1) else
    sum (acc + stop(i) + stop(i+1) + stop(i+2) + stop(i+3) +
               stop(i+4) + stop(i+5) + stop(i+6) + stop(i+7)) (i+8) 
  in 
  sum 0 a

;;;;

let n = 2

print_string "____\n"
print_int (sum_syracuse_par1 1 n);;
print_int (sum_syracuse_par2 1 n);;
print_int (sum_syracuse_par4 1 n);;
print_int (sum_syracuse_par8 1 n);;