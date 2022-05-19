circuit sum_left l =
  let fold_left f accu l =
    let rec aux accu l =
      match l with
      | [] -> accu
      | a::l -> aux (f accu a) l 
    in aux accu l
  in
  let add x y = x + y in
  fold_left add 0 l ;;


circuit sum_right l =
  let fold_right f accu l =
    let rec aux l =
      match l with
      | [] -> accu
      | a::l -> f a (aux l)
    in aux l
  in
  let add x y = x + y in
  fold_right add 0 l


;;;;

let n = 100 ;;

let l = List.init n (fun i -> i) ;;

let main () =
  print_int (sum_left l);
  print_string "\n";
  print_int (sum_right l) ;;

main ();;
