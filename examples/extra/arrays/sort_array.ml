circuit sort_array a =
  let size = array_length a in
  let rec loop1 i =
    if i < size then 
      let rec loop2 j =
        if j > 0 then 
          let x = a.(j-1) and y = a.(j) in
          if x > y then 
            let _ = a.(j-1) <- y in
            let _ = a.(j) <- x in
            loop2 (j-1)
          else loop1 (i+1)
        else loop1 (i+1)
      in 
      loop2 i
    else ()
  in
  loop1 1

;;;;

let a = [|7;5;3;9;6;8;1;2;4|];;

sort_array a;;

Array.iter print_int a;;