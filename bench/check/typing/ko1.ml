circuit f x = 
  let rec f g = 5 in
  let h x = x in
  f h

(* in circuit f [from line 2, characters 17, to line 4 characters 58]
   Error: Recursive functions must be first-order
   but f has type (('a4->'a4)->int). *)
;;;;