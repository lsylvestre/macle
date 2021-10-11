circuit s(a,i,x) =
  let f(j) =
    a.(i+j) <- x
  in f(2)

;;;

let a = [|1;2;3;4;5|];;

s a 1 42;;

Array.iter (fun x -> print_int x; print_string ";") a