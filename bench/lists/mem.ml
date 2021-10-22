circuit rec mem(y,l) =
  match l with
  | [] -> false
  | x::l' -> if x = y then true else mem(y,l')  ;;

circuit sum(l) =
  let rec aux (acc,l) = 
    match l with
    | [] -> acc
    | x::l' -> aux(acc+x,l') 
  in aux(0,l) ;;
  
circuit length(l) = 
  let rec aux (acc,l) = 
    match l with
    | [] -> acc
    | x::l' -> aux(acc+1,l') 
  in aux(0,l)

;;;;;;;

let l = [1;2;3;5;42;7] ;;

if mem 42 l then print_int 1 else print_int 0;;

print_string "\n";;

print_int @@ sum(l);;

print_int @@ length(l);;