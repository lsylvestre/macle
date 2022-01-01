circuit sum_classic a r =
  r := 0;
  let z = array_length a in
  let rec aux i =
    if i >= z then () else
    ( r := ((!r) + a.(i));
      aux (i+1) )
  in
  aux 0
;;

circuit sum1 a r =
  r := 0;
  let f pk = 
    let add x y = x + y in
    let x = reduce add 0 pk in
    r := ((!r) + x)
  in
  array_iter_by 1 f a
;;

circuit sum10 a r =
  r := 0;
  let f pk = 
    let add x y = x + y in
    let x = reduce add 0 pk in
    r := ((!r) + x)
  in
  array_iter_by 10 f a
;;

circuit sum50 a r =
  r := 0;
  let f pk = 
    let add x y = x + y in
    let x = reduce add 0 pk in
    r := ((!r) + x)
  in
  array_iter_by 50 f a ;;

circuit sum100 a r =
  r := 0;
  let f pk = 
    let add x y = x + y in
    let x = reduce add 0 pk in
    r := ((!r) + x)
  in
  array_iter_by 100 f a

;;;;;


let r = (ref 0);;

sum_classic (Array.init 10000 (fun x -> x)) r;;

print_int @@ !r;;