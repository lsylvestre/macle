circuit sum_list(l) = 
  let add(s,x) = s + x in
  list_fold_left(add,0,l) ;;

circuit sum_array(a) = 
  let add(s,x) = s + x in
  array_fold_left(add,0,a) ;;

circuit sum_array2(a) = 
  let add(s,x) = s + x in
  let y = a in
  let n = array_length y in
  let rec q(acc,i) = 
    if i >= n then acc else
    let x = y.(i) in
    let acc2 = add(acc,x) in
    q(acc2,i+1) in
  q(0,0)

;;

circuit len(a) =
  array_length a  ;;

circuit array_inc(a) = 
  let inc(x) = x + 1 in
  array_map_by(4,inc,a)

;;;


let print_int = Serial.write_int ;;
let print_string = Serial.write_string ;;

print_int (sum_list [1;2;3;4;5;6;7;8;9;10]);;
print_string "\n";;
print_int (sum_array [|1;2;3;4;5;6;7;8;9;10|]);;
print_string "\n";;

print_int (sum_array2 [|1;2;3;4;5;6;7;8;9;10|]);;
print_string "\n";;

print_int (len [|1;2;3;4;5;6;7;8;9;10|]);;
print_string "\n";;

let a = [|1;2;3;4;5;6;7;8|] ;;
array_inc a;;
print_int (a.(0)) ;; print_string " ";;
print_int (a.(1)) ;; print_string " ";;
print_int (a.(2)) ;; print_string " ";;
print_int (a.(3)) ;; print_string " ";;
print_int (a.(4)) ;; print_string " ";;
print_int (a.(5)) ;; print_string " ";;
print_int (a.(6)) ;; print_string " ";;
print_int (a.(7)) ;; 
print_string "\n";;