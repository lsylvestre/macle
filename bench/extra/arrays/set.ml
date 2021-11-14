(*circuit g(x) =
 x := 42*)

circuit f(x) =
 x.(1) <- 42


;;;;;

(*
let r = ref 0;;
g r;;
print_int (!r);;
print_string "====\n";;
*)
let a = [|1;2;3;4;5;6;7;8;9|];;

print_int @@ f a;;

Array.iter (fun x ->
  print_int x; print_string " ") a;;