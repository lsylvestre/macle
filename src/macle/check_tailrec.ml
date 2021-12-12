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
  let rec aux ~tailpos lenv env (desc,loc) =
    match desc with
    | Var _ | Const _ -> ()
    | Unop(_,e) -> 
        aux ~tailpos:false lenv env e
    | Binop((Or|And),e1,e2) -> 
        aux ~tailpos:false lenv env e1;
        aux ~tailpos lenv env e2
    | Binop(_,e1,e2) -> 
        aux ~tailpos:false lenv env e1;
        aux ~tailpos:false lenv env e2
    | If(e1,e2,e3) -> 
        aux ~tailpos:false Env.empty (Env.union lenv env) e1;
        aux ~tailpos lenv env e2; 
        aux ~tailpos lenv env e3
    | Let(bs,e) ->
        List.iter (fun (_,e) -> aux ~tailpos:false Env.empty (Env.union lenv env) e) bs; 
        aux ~tailpos (remove_bindings bs lenv) (remove_bindings bs env) e
    | LetFun(((x,_),e1),e2) -> 
        aux ~tailpos lenv env e1;
        aux ~tailpos (Env.remove x lenv) (Env.remove x env) e2
    | LetRec(bs,e) ->
        aux ~tailpos lenv env e;
        let lenv' = extend_bindings bs lenv in
        List.iter (fun (_,e) -> aux ~tailpos:true lenv' env e) bs
    | App(x,es) -> 
        if (Env.mem x lenv && not tailpos) || Env.mem x env 
        then raise (Not_tailrec (desc,loc));
        List.iter (aux ~tailpos:false lenv env) es
    | Match(e,cases) ->
        aux ~tailpos:false lenv env e;
        List.iter (fun (_,xs,e) -> 
                     aux ~tailpos (remove_patern_vars xs env) 
                                  (remove_patern_vars xs lenv) e) cases
    | Raise _ -> 
        ()
    | CamlPrim c -> 
        (match c with 
         | ArrayAccess { arr ; idx } ->
            aux ~tailpos:false lenv env arr;
            aux ~tailpos:false lenv env idx
         | (RefAccess e | ArrayLength e) -> 
            aux ~tailpos:false lenv env e
         | ArrayAssign{arr;idx;e} -> 
            aux ~tailpos:false lenv env arr;
            aux ~tailpos:false lenv env idx;
            aux ~tailpos:false lenv env e
         | RefAssign {r;e} ->
            aux ~tailpos:false lenv env r; 
            aux ~tailpos:false lenv env e
         | ArrayFoldLeft (_,e1,e2) ->  
            aux ~tailpos:false lenv env e1; 
            aux ~tailpos:false lenv env e2
         | ArrayMapBy(_,_,e) -> 
            aux ~tailpos:false lenv env e)
in aux ~tailpos:true Env.empty Env.empty e

open Loc

let check_tailrec ?(fmt=Format.std_formatter) ({x;decoration=loc;e} : circuit) : unit = 
  try tailrec e with
  | Not_tailrec ((_,loc) as e) ->
    error x loc @@
        fun fmt () -> 
          Format.fprintf fmt "The recursive call: %a should be in tail position." 
             Pprint_ast.PP_MACLE.pp_exp e
