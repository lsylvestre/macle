circuit sum_classic a =
  let add x y = x + y in
  array_fold_left add 0 a ;;


circuit sum_pk1 a =
  let add x y = x + y in
  let f acc pk =
    reduce add acc pk
  in
  array_reduce_by 1 f 0 a ;;


circuit sum_pk10 a =
  let add x y = x + y in
  let f acc pk =
    reduce add acc pk
  in
  array_reduce_by 10 f 0 a ;;

circuit sum_pk20 a =
  let add x y = x + y in
  let f acc pk =
    reduce add acc pk
  in
  array_reduce_by 20 f 0 a;;

circuit sum_pk50 a =
  let add x y = x + y in
  let f acc pk =
    reduce add acc pk
  in
  array_reduce_by 50 f 0 a;;

circuit sum_pk100 a =
  let add x y = x + y in
  let f acc pk =
    reduce add acc pk
  in
  array_reduce_by 100 f 0 a

;;;;;

let a = [| 1;2;3;4;5;6;7;8;9;10;
           (* ... *)
        |]
;;

print_string "\n--> ";;
print_int @@ sum_classic a ;;
