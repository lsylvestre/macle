circuit f x = 
  let rec g x =
    if (let rec h y = g y in true) then 1 else 2
  in 
  42

(* in circuit f [line 3, characters 53-56]
   Error: The recursive call: (g y)
   should be in tail position. *)
   
;;;;