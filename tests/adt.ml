type t = 
  A1 of int 
| C1 
| C2 
| A2 of int * int 
| C3 
| A3 of int * t 
| C4 
| C5 
| A4 of int * int * t 

;;

circuit f x =
  let rec aux acc x =
    match x with
    | A1(n) -> 10000 + n
    | C1 -> 20000
    | C2 -> 30000
    | A2(n,m) -> 40000 + n + m
    | C3 -> 50000
    | A3(n,x) -> aux (acc + 60000 + n) x
    | C4 -> 70000
    | C5 -> 80000
    | A4(n,m,x) -> aux (acc + 60000 + n + m) x
  in aux 0 x

;;;;


print_int (f @@ A1(42));;
print_string "\n";;
print_int (f @@ A2(10,32));;
print_string "\n";;
print_int (f @@ A3(42,A1(42)));;
print_string "\n";;
print_int (f @@ A4(10,32,A1(42)));;
print_int (f @@ C1);;
print_string "\n";;
print_int (f @@ C2);;
print_string "\n";;
print_int (f @@ C3);;
print_string "\n";;
print_int (f @@ C4);;
print_string "\n";;
print_int (f @@ C5);;
print_string "\n";;