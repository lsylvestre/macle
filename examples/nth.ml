circuit nth n l = 
  let rec loop i l =
    match l with
    | [] -> raise (Failure "nth")
    | x::t -> 
       if i = n then x else loop (i+1) t
  in loop 0 l

;;;;


let main n =
  try 
    print_string (nth n ["foo";"bar";"baz"])
  with
  | Failure msg -> print_string msg ;;

main 2 ;;
