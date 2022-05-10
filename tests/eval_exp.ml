type exp = 
    Int of int 
  | Var of int
  | Add of exp * exp ;;

circuit eval_exp env e =
  let rec eval e =
    match e with
    | Int(n) -> n
    | Var(k) -> env.(0)
    | Add(e1,e2) -> 
        let x1 = eval e1 in
        let x2 = eval e2 in x1 + x2
  in 
  eval e

;;;;

let gen_leaf x =
  if Random.bool () then Int(Random.int 10) else Var x ;;

let rec gen_exp n x = 
  if n = 0 || Random.int n = 0 then gen_leaf x else Add(gen_exp (n-1) x,gen_exp (n-1) x) ;;


let e = gen_exp 10 0;;
let env = [|100|] ;;

print_int (eval_exp env e);;