circuit sum_classic a =
  let fib n = 
    let rec f a b n = 
      if n <= 0 then a else f b (a+b) (n-1) in 
      f 1 1 n 
  in
  let f acc x = acc + fib x in
  array_fold_left f 0 a ;;


circuit sum_pk1 a =
  let add x y = x + y in
  let fib n = 
    let rec f a b n = 
      if n <= 0 then a else f b (a+b) (n-1) in 
      f 1 1 n 
  in
  let f acc pk =
    reduce add acc (map fib pk)
  in
  array_reduce_by 1 f 0 a ;;


circuit sum_pk10 a =
  let add x y = x + y in
  let fib n = 
    let rec f a b n = 
      if n <= 0 then a else f b (a+b) (n-1) in 
      f 1 1 n 
  in
  let f acc pk =
    reduce add acc (map fib pk)
  in
  array_reduce_by 10 f 0 a ;;

circuit sum_pk20 a =
  let add x y = x + y in
  let fib n = 
    let rec f a b n = 
      if n <= 0 then a else f b (a+b) (n-1) in 
      f 1 1 n 
  in
  let f acc pk =
    reduce add acc (map fib pk)
  in
  array_reduce_by 20 f 0 a ;;

circuit sum_pk50 a =
  let add x y = x + y in
  let fib n = 
    let rec f a b n = 
      if n <= 0 then a else f b (a+b) (n-1) in 
      f 1 1 n 
  in
  let f acc pk =
    reduce add acc (map fib pk)
  in
  array_reduce_by 50 f 0 a ;;

circuit sum_pk100 a =
  let add x y = x + y in
  let fib n = 
    let rec f a b n = 
      if n <= 0 then a else f b (a+b) (n-1) in 
      f 1 1 n 
  in
  let f acc pk =
    reduce add acc (map fib pk)
  in
  array_reduce_by 100 f 0 a

;;;;;

let a = [| 10;10;10;10;10;10;10;10;10;10;
           (* ... *)
        |]
;;

print_string "\n--> ";;
print_int @@ sum_classic a ;;
