circuit list_length (l) = 
  let rec aux (a,l) =
    match l with
    | [] -> a 
    | x::l' -> aux (1+a,l') in
  aux(0,l)

;;;;

let n = 100*100 ;; 

let l = List.init n (fun x -> x);; 

print_int (list_length l) ;;
