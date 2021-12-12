circuit map_fibonacci1 a =
  let fibonacci n = 
    let rec aux a b n =
      if n < 1 then a else aux b (a+b) (n-1) 
    in aux 0 1 n 
  in
  array_map_by 1 fibonacci a;;

circuit map_fibonacci2 a =
  let fibonacci n = 
    let rec aux a b n =
      if n < 1 then a else aux b (a+b) (n-1) 
    in aux 0 1 n 
  in
  array_map_by 2 fibonacci a;;

circuit map_fibonacci4 a =
  let fibonacci n = 
    let rec aux a b n =
      if n< 1 then a else aux b (a+b) (n-1) 
    in aux 0 1 n 
  in
  array_map_by 4 fibonacci a;;

circuit map_fibonacci8 a =
  let fibonacci n = 
    let rec aux a b n =
      if n < 1 then a else aux b (a+b) (n-1) 
    in aux 0 1 n 
  in
  array_map_by 8 fibonacci a;;

circuit map_fibonacci16 a =
  let fibonacci n = 
    let rec aux a b n =
      if n < 1 then a else aux b (a+b) (n-1) 
    in aux 0 1 n 
  in
  array_map_by 16 fibonacci a;;

circuit map_fibonacci32 a =
  let fibonacci n = 
    let rec aux a b n =
      if n < 1 then a else aux b (a+b) (n-1) 
    in aux 0 1 n 
  in
  array_map_by 32 fibonacci a;;

circuit map_fibonacci64 a =
  let fibonacci n = 
    let rec aux a b n =
      if n < 1 then a else aux b (a+b) (n-1) 
    in aux 0 1 n 
  in
  array_map_by 64 fibonacci a


;;;;
(* programme OCaml *)
let size = 1*64;; 
let n = 10;;

let a = Array.init size (fun i -> i mod n);;

map_fibonacci64 a;;