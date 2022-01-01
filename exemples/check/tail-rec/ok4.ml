circuit f x = 
    (let rec g x = g x in 42) + 1

(* in circuit f [line 4, characters 65-68]
   Error: The recursive call: (h y)
   should be in tail position. *)
   
;;;;