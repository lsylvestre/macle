
circuit sum_array(a) =
  let rec aux(acc,i) =
    if i < 10 then aux(acc+a.(i),i+1) else acc in aux(0,0) 

;;;;

let a = [|1;2;3;4;5;6;7;8;9;10|];;

print_int @@ sum_array a;;
