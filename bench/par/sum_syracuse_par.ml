
circuit div2(a) =
  a / 2 ;;

circuit mod2(a) =
  a mod 2 ;;

circuit dur(n) =
  let rec syracuse(t,n) =
    if n <= 1 then t else
    if n mod 2 = 0 then syracuse(t+1,n/2) 
                   else syracuse(t+1,3*n+1) 
    in
    syracuse(0,n) ;;

circuit sum_syracuse_par1(a,b) = 
  let dur(n) = 
    let rec syracuse(t,n) =
    if n <= 1 then t else
    if n mod 2 = 0 then syracuse(t+1,n/2) 
                   else syracuse(t+1,3*n+1) 
    in
    syracuse(0,n) in
  let rec sum(acc,i) = 
    if i >= b then acc else sum(acc+dur(i),i+1) in
    sum(0,a) ;;


circuit sum_syracuse_par2(a,b) = 
  let dur(n) = 
    let rec syracuse(t,n) =
    if n <= 1 then t else
    if n mod 2 = 0 then syracuse(t+1,n/2) 
                   else syracuse(t+1,3*n+1) 
    in
    syracuse(0,n) in
  let rec sum(acc,i) = 
    if i >= b then acc else
    if i > b-8 then sum(acc+dur(i),i+1) 
    else sum (acc+dur(i)+dur(i+1),
              i+2) 
    in
    sum(0,a)
;;

circuit sum_syracuse_par4(a,b) = 
  let dur(n) = 
    let rec syracuse(t,n) =
    if n <= 1 then t else
    if n mod 2 = 0 then syracuse(t+1,n/2) 
                   else syracuse(t+1,3*n+1) 
    in
    syracuse(0,n) in
  let rec sum(acc,i) = 
    if i >= b then acc else
    if i > b-4 then sum(acc+dur(i),i+1) 
    else
      sum (acc +
              (dur(i) +
              dur(i+1) +
              dur(i+2) +
              dur(i+3)),i+4) 
    in
    sum(0,a)
;;

circuit sum_syracuse_par8(a,b) = 
  let dur(n) = 
    let rec syracuse(t,n) =
    if n <= 1 then t else
    if n mod 2 = 0 then syracuse(t+1,n/2) 
                   else syracuse(t+1,3*n+1) 
    in
    syracuse(0,n) in
  let rec sum(acc,i) = 
    if i >= b then acc else
    if i > b-8 then sum(acc+dur(i),i+1) 
    else
      sum (acc +
              (dur(i) +
              dur(i+1) +
              dur(i+2) +
              dur(i+3) +
              dur(i+4) +
              dur(i+5) +
              dur(i+6) +
              dur(i+7)),i+8) 
    in
    sum(0,a)

;;

circuit sum_syracuse_par16(a,b) = 
  let dur(n) = 
    let rec syracuse(t,n) =
    if n <= 1 then t else
    if n mod 2 = 0 then syracuse(t+1,n/2) 
                   else syracuse(t+1,3*n+1) 
    in
    syracuse(0,n) in
  let rec sum(acc,i) = 
    if i >= b then acc else
    if i > b-16 then sum(acc+dur(i),i+1) 
    else
      sum (acc +
              (dur(i) +
              dur(i+1) +
              dur(i+2) +
              dur(i+3) +
              dur(i+4) +
              dur(i+5) +
              dur(i+6) +
              dur(i+7) +
              dur(i+8) +
              dur(i+9) + 
              dur(i+10) +
              dur(i+11) +
              dur(i+12) +
              dur(i+13) +
              dur(i+14) +
              dur(i+15)),i+16) 
    in
    sum(0,a) ;;

circuit sum_syracuse_par32(a,b) = 
  let dur(n) = 
    let rec syracuse(t,n) =
    if n <= 1 then t else
    if n mod 2 = 0 then syracuse(t+1,n/2) 
                   else syracuse(t+1,3*n+1) 
    in
    syracuse(0,n) in
  let rec sum(acc,i) = 
    if i >= b then acc else
    if i > b-32 then sum(acc+dur(i),i+1) 
    else
      sum (acc +
              (dur(i) +
              dur(i+1) +
              dur(i+2) +
              dur(i+3) +
              dur(i+4) +
              dur(i+5) +
              dur(i+6) +
              dur(i+7) +
              dur(i+8) +
              dur(i+9) + 
              dur(i+10) +
              dur(i+11) +
              dur(i+12) +
              dur(i+13) +
              dur(i+14) +
              dur(i+15) +
              dur(i+16) +
              dur(i+17) +
              dur(i+18) +
              dur(i+19) + 
              dur(i+20) +
              dur(i+21) +
              dur(i+22) +
              dur(i+23) +
              dur(i+24) +
              dur(i+25) +
              dur(i+26) +
              dur(i+27) +
              dur(i+28) +
              dur(i+29) + 
              dur(i+30) +
              dur(i+31)
              ),i+32) 
    in
    sum(0,a)
;;


circuit sum_syracuse_par64(a,b) = 
  let dur(n) = 
    let rec syracuse(t,n) =
    if n <= 1 then t else
    if n mod 2 = 0 then syracuse(t+1,n/2) 
                   else syracuse(t+1,3*n+1) 
    in
    syracuse(0,n) in
  let rec sum(acc,i) = 
    if i >= b then acc else
    if i > b-64 then sum(acc+dur(i),i+1) 
    else
      sum (acc +
              (dur(i) +
              dur(i+1) +
              dur(i+2) +
              dur(i+3) +
              dur(i+4) +
              dur(i+5) +
              dur(i+6) +
              dur(i+7) +
              dur(i+8) +
              dur(i+9) + 
              dur(i+10) +
              dur(i+11) +
              dur(i+12) +
              dur(i+13) +
              dur(i+14) +
              dur(i+15) +
              dur(i+16) +
              dur(i+17) +
              dur(i+18) +
              dur(i+19) + 
              dur(i+20) +
              dur(i+21) +
              dur(i+22) +
              dur(i+23) +
              dur(i+24) +
              dur(i+25) +
              dur(i+26) +
              dur(i+27) +
              dur(i+28) +
              dur(i+29) + 
              dur(i+30) +
              dur(i+31) +
              dur(i+32) +
              dur(i+33) +
              dur(i+34) +
              dur(i+35) +
              dur(i+36) +
              dur(i+37) +
              dur(i+38) +
              dur(i+39) + 
              dur(i+40) +
              dur(i+41) +
              dur(i+42) +
              dur(i+43) +
              dur(i+44) +
              dur(i+45) +
              dur(i+46) +
              dur(i+47) +
              dur(i+48) +
              dur(i+49) + 
              dur(i+50) +
              dur(i+51) +
              dur(i+52) +
              dur(i+53) +
              dur(i+54) +
              dur(i+55) +
              dur(i+56) +
              dur(i+57) +
              dur(i+58) +
              dur(i+59) + 
              dur(i+60) +
              dur(i+61) +
              dur(i+62) +
              dur(i+63)
              ),i+64) 
    in
    sum(0,a)

;;;;

let print n = print_int n ; print_string "\n";;


print @@ div2 4;;

print @@ div2 8;;
print @@ div2 7;;

print @@ mod2 7;;

print @@ mod2 55;;

print @@ mod2 60;;

print @@ dur 29;;

let a = 1 ;;
let b = 100001 ;;

print @@ sum_syracuse_par1 a b ;;

print @@ sum_syracuse_par2 a b ;;

print @@ sum_syracuse_par4 a b ;;

print @@ sum_syracuse_par8 a b ;;

print @@ sum_syracuse_par16 a b ;;
(*
print @@ sum_syracuse_par32 1 100000 ;;

print @@ sum_syracuse_par64 1 100000 ;;*)