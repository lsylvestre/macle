circuit map_sum1 a =
  let sum n = 
    let rec aux a n =
      if n < 1 then a else aux (a+n) (n-1) 
    in aux 0 n 
  in
  array_map_by 1 sum a;;

circuit map_sum2 a =
  let sum n = 
    let rec aux a n =
      if n < 1 then a else aux (a+n) (n-1) 
    in aux 0 n 
  in
  array_map_by 2 sum a;;


circuit map_sum4 a =
  let sum n = 
    let rec aux a n =
      if n < 1 then a else aux (a+n) (n-1) 
    in aux 0 n 
  in
  array_map_by 4 sum a;;

circuit map_sum8 a =
  let sum n = 
    let rec aux a n =
      if n < 1 then a else aux (a+n) (n-1) 
    in aux 0 n 
  in
  array_map_by 8 sum a;;

circuit map_sum16 a =
  let sum n = 
    let rec aux a n =
      if n < 1 then a else aux (a+n) (n-1) 
    in aux 0 n 
  in
  array_map_by 16 sum a;;

circuit map_sum32 a =
  let sum n = 
    let rec aux a n =
      if n < 1 then a else aux (a+n) (n-1) 
    in aux 0 n 
  in
  array_map_by 32 sum a;;

circuit map_sum64 a =
  let sum n = 
    let rec aux a n =
      if n < 1 then a else aux (a+n) (n-1) 
    in aux 0 n 
  in
  array_map_by 64 sum a

;;;;
(* programme OCaml *)
let size = 1*64;; 
let n = 10;;

let a = Array.init size (fun i -> i mod n);;

map_sum64 a;;