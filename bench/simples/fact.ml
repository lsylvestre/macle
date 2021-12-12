(* Macle circuit *)

circuit fact n = 
  let rec aux acc n = 
    if n <= 0 then acc else aux (acc*n) (n-1) in
  aux 1 n

;;; 

(* OCaml program *)

print_int @@ fact 6;;