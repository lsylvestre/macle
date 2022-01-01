circuit sum a r =
  (* r := 0;*)
  let f pk = 
    let add x y = x + y in
    let inc x = x + 1 in
    let w = map inc pk in
    let z = reduce add 0 (map add w w) in
    r := ((!r) + z)
  in
  array_iter_by 10 f a

;;;;;


let r = (ref 0);;

sum (Array.init 100 (fun x -> x)) r;;

print_int @@ !r;;