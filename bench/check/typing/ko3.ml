circuit f x = 
  let rec id x = x in id id

(* in circuit f [line 2, characters 37-42]
   Error: cannot unify types ('a2->'a2) and 'a2. *)

;;;;