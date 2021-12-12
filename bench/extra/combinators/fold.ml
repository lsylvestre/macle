circuit list_sum_twice l =
  let rec plus a b =  
      if b <= 0 then a else plus (a+1) (b-1) in
  let plus_twice a b =
     let x = plus a b
     and y = plus a b in
     x + y in 
  list_fold_left plus_twice 0 l ;;

circuit array_sum_twice arr =
  let rec plus a b =  
      if b <= 0 then a else plus (a+1) (b-1) in
  let plus_twice a b =
     let x = plus a b
     and y = plus a b in
     x + y in 
  array_fold_left plus_twice 0 arr


;;;

let print_list l = 
 List.iter (fun n -> print_int n; print_string ",") l;
  print_string "---------\n";;

let print_array a = 
  Array.iter (fun n -> print_int n; print_string ",") a;
  print_string "---------\n";;


let l = [1;2;3;4;5;6;7;8;9;10] ;;

print_list l;;

let n = list_sum_twice(l);;

print_string "==>";;

print_int(n);;
print_string "\n\n";;

let a = [|1;2;3;4;5;6;7;8;9;10|] ;;

print_array a;;

let n = array_sum_twice a;;

print_string "==>";;

print_int n;;