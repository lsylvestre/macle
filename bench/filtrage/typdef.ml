
type t = Nil | Cons of int * t ;;

circuit first l =
  match l with
  | Nil -> 0
  | Cons(x,_) -> x ;;

circuit sum l =
  let rec aux a l =
    match l with
    | Nil -> a
    | Cons(x,l') -> aux (x+a) l' 
  in
  aux 0 l ;;

circuit rec mem x l =
    match l with
    | Nil -> false
    | Cons(y,l') -> if x = y then true else mem x l'


;;;;

print_int @@ first (Cons(3,Cons(4,Nil)));;
print_string "\n";;
print_int @@ sum (Cons(3,Cons(4,Cons(5,Nil))));;
print_string "\n";;

let f b = if b then 1 else 0 ;;

print_int @@ f @@ mem 4 (Cons(4,Nil)) ;;
print_string "\n";;
print_int @@ f @@ mem 4 (Cons(3,Nil)) ;;
print_string "\n";;
print_int @@ f @@ mem 4 (Cons(3,Cons(4,Nil))) ;;
print_string "\n";;
print_int @@ f @@ mem 4 (Cons(3,Cons(5,Nil))) ;;*)