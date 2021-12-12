
circuit rev_array a =
  let rec aux i j =
    if i >= j then () else 
      let v = a.(i) in
      let _ = a.(i) <- a.(j) in
      let _ = a.(j) <- v in
      aux (i+1) (j-1)
  in
  aux 0 (array_length a-1)

;;;;

let a = [|1;2;3;4;5;6;7;8;9|];;

rev_array a;;

Array.iter print_int a;;