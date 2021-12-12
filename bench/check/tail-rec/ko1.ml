circuit f x = 
  let rec g x =
    let v = g x in
    42
  in 4

(* in circuit f [line 3, characters 43-46]
   Error: The recursive call: (g x)
   should be in tail position. *)
;;;;