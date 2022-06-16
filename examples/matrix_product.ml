circuit matrix_product m1 m2 dst =
  let x0 = array_length m1
  and y0 = array_length m2 in
  let y1 = if y0 = 0 then 0 else array_length (m2.(0)) in
  for i = 0 to x0-1 do
    for j = 0 to y1-1 do
      for k = 0 to y0-1 do
       (dst.(i)).(j) <- ((dst.(i)).(j) + (m1.(i)).(k) * (m2.(k)).(j))
      done
    done
  done

;;;;

let n = 30;;

let dst = Array.make_matrix n n 0;;

let init n = 
  let m = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
         m.(i).(j) <- Random.int n
    done
  done;
  m;;

let m1 = init n ;;
let m2 = init n ;;

matrix_product m1 m2 dst ;;


let show_matrix m =
  for i = 0 to Array.length m - 1 do
    for j = 0 to Array.length m - 1 do
      print_int m.(i).(j);
      print_string ";"
    done;
    print_string "\n"
  done

;;

if n <= 40 then show_matrix dst;;
