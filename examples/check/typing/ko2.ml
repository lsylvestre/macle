circuit f g = 
  g 4

(* in circuit f [from line 1, characters 0, to line 2 characters 20]
   Error: Circuits must be first-order
   but f has type ((int->'a2)->'a2). *)

;;;;