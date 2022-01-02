circuit sum a r =
  r := 0;
  let f pk = 
    let add x y = x + y in
    let inc x = x + 1 in 
    let x = reduce add 0 (map inc (map inc (map inc (map inc (map inc (map inc pk)))))) in
    r := ((!r) + x)
  in
  array_iter_by 10 f a
;;;;;

let r = (ref 0);;

sum (Array.init 10000 (fun x -> x)) r;;

print_int @@ !r;;