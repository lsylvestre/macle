circuit dur(n) =
  let rec syracuse(t,n) =
    if n <= 1 then t else
    if n mod 2 = 0 then syracuse(t+1,n/2) 
                   else syracuse(t+1,3*n+1) 
    in
    syracuse(0,n)

;;;;

let print n = print_int n ; print_string "\n" ;;


print @@ dur 29;;