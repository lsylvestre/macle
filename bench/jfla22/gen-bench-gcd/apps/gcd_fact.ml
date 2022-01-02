open Platform ;;

let print_int = Serial.write_int ;;
let print_string = Serial.write_string ;;

Timer.init() ;;

let chrono f =
  let t1 = Timer.get_us () in
  let _ = f () in
  let t2 = Timer.get_us () in
  t2-t1


(* OCaml program *)

let n = 10;;

for i = 1 to n do
  print_int @@ gcd i (fact i)
done ;;
