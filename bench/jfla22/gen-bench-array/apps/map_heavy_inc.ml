open Platform ;;

let print_int = Serial.write_int ;;
let print_string = Serial.write_string ;;

Timer.init() ;;

let chrono f =
  let t1 = Timer.get_us () in
  let _ = f () in
  let t2 = Timer.get_us () in
  t2-t1


(* programme OCaml *)
let size = 1*64;; 
let n = 1000*1000;;

let a = Array.init size (fun _ -> Random.int n);;

map_heavy_inc1 a;;
