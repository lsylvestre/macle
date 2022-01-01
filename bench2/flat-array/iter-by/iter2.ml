circuit sum a r =
  (* r := 0;*)
  let f pk = 
    let add x y = x + y in
    let x = reduce add 0 pk in
    r := ((!r) + x)
  in
  array_iter_by 20 f a

;;;;;


let r = (ref 0);;

sum (Array.init 100 (fun x -> x)) r;;

print_int @@ !r;;