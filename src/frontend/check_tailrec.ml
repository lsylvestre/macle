open Ast
open MACLE

(* Env contains names of recursive functions *)

module Env = Set.Make(String)

let extend_bindings bs env = 
  List.fold_right (fun ((x,_),_) env -> Env.add x env) bs env 

let remove_bindings bs env =
  List.fold_right (fun ((x,_),_) env -> Env.remove x env) bs env     

let remove_patern_vars ps env =
  List.fold_right 
    (fun (p,_) env -> 
       match p with 
       | Some x -> Env.remove x env 
       | _ -> env) ps env     


exception Not_tailrec of exp

(* raises [Not_tailrec e] when a recursive call [e] is not in tail position *)

let tailrec e =
  let rec aux ~tailpos env (desc,loc) =
    match desc with
    | Var _ | Const _ -> ()
    | Unop(_,e) -> 
        aux ~tailpos:false env e
    | Binop(_,e1,e2) -> 
        aux ~tailpos:false env e1;
        aux ~tailpos:false env e2
    | If(e1,e2,e3) -> 
        aux ~tailpos:false env e1;
        aux ~tailpos env e2; 
        aux ~tailpos env e3
    | Let(bs,e) -> 
        List.iter (fun (_,e) -> aux ~tailpos env e) bs; 
        let env' = remove_bindings bs env in
        aux ~tailpos env' e
    | LetFun(((x,_),e1),e2) -> 
        aux ~tailpos env e1;
        aux ~tailpos (Env.remove x env) e2
    | LetRec(bs,e) ->
        aux ~tailpos env e;
        let env' = extend_bindings bs env in 
        List.iter (fun (_,e) -> aux ~tailpos env' e) bs
    | App(x,es) -> 
        if Env.mem x env && not tailpos then raise (Not_tailrec (desc,loc));
        List.iter (aux ~tailpos:false env) es
    | Match(e,cases) ->
        aux ~tailpos:false env e;
        List.iter (fun (_,xs,e) -> 
                     aux ~tailpos (remove_patern_vars xs env) e) cases
    | Raise _ -> 
        ()
    | CamlPrim c -> 
        (match c with 
         | ArrayAccess { arr ; idx } ->
            aux ~tailpos:false env arr;
            aux ~tailpos:false env idx
         | (RefAccess e | ArrayLength e | ListHd e | ListTl e) -> 
            aux ~tailpos:false env e
         | ArrayAssign{arr;idx;e} -> 
            aux ~tailpos:false env arr;
            aux ~tailpos:false env idx;
            aux ~tailpos:false env e
         | RefAssign {r;e} ->
            aux ~tailpos:false env r; 
            aux ~tailpos:false env e
         | ListFoldLeft (_,e1,e2) 
         | ArrayFoldLeft (_,e1,e2) ->  
            aux ~tailpos:false env e1; 
            aux ~tailpos:false env e2
         | ArrayMapBy(_,_,e) -> 
            aux ~tailpos:false env e)
in aux ~tailpos:true Env.empty e

open Loc

let check_tailrec ?(fmt=Format.std_formatter) ({x;decoration=loc;e} : circuit) : unit = 
  try tailrec e with
  | Not_tailrec ((_,loc) as e) ->
    error x loc @@
        fun fmt () -> 
          Format.fprintf fmt "The recursive call: %a should be in tail position." 
             Pprint_ast.PP_MACLE.pp_exp e
