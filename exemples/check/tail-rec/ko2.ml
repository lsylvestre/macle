circuit f x = 
  let rec h y = 
    let rec g x = 
      let v = h y in 
      5 
    in 42
  in 4

(* in circuit f [line 4, characters 65-68]
   Error: The recursive call: (h y)
   should be in tail position. *)
   
;;;;