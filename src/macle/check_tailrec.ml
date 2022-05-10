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
       | Some x ->
           Env.remove x env
       | _ ->
           env) ps env


exception Not_tailrec of exp

let app ~tailpos f env lenv desc loc =
  if (Env.mem f lenv && not tailpos) || Env.mem f env
  then raise (Not_tailrec (desc,loc))

(* raises [Not_tailrec e] when a recursive call [e] is not in tail position *)

let tailrec e =
  let rec aux ~tailpos lenv env (desc,loc) =
    match desc with
    | Var _ | Const _ -> ()
    | Unop(_,e) ->
        aux ~tailpos:false lenv env e
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
    | App(f,es) ->
        app ~tailpos f env lenv desc loc;
        List.iter (aux ~tailpos:false lenv env) es
    | Match(e,cases) ->
        aux ~tailpos:false lenv env e;
        List.iter (fun (_,xs,e) ->
            aux ~tailpos (remove_patern_vars xs env)
              (remove_patern_vars xs lenv) e)
        @@ cases
    | Raise _ ->
        ()
    | CamlPrim c ->
        (match c with
         | ArrayAccess { arr ; idx } ->
             aux ~tailpos:false lenv env arr;
             aux ~tailpos:false lenv env idx
         | (RefAccess e | ArrayLength e | Ref e) ->
             aux ~tailpos:false lenv env e
         | ArrayAssign{arr;idx;e} ->
             aux ~tailpos:false lenv env arr;
             aux ~tailpos:false lenv env idx;
             aux ~tailpos:false lenv env e
         | RefAssign {r;e} ->
             aux ~tailpos:false lenv env r;
             aux ~tailpos:false lenv env e
         | ArrayMake {size;e} ->
             aux ~tailpos:false lenv env size;
             aux ~tailpos:false lenv env e)
    | PacketPrim c ->
        (match c with
         | PkMake es ->
             List.iter (aux ~tailpos:false lenv env) es
         | PkGet (e,idx) ->
             aux ~tailpos:false lenv env e;
             aux ~tailpos:false lenv env idx
         | PkSet (_,idx,e) ->
             aux ~tailpos:false lenv env idx;
             aux ~tailpos:false lenv env e
         | ToPacket(e,idx,_) ->
             aux ~tailpos:false lenv env e;
             aux ~tailpos:false lenv env idx
         | OfPacket(e1,e2,idx,_) ->
             aux ~tailpos:false lenv env e1;
             aux ~tailpos:false lenv env e2;
             aux ~tailpos:false lenv env idx
         | PkMap((xs,e),es) ->
             aux ~tailpos:false lenv env e;
             List.iter (aux ~tailpos:false lenv env) es
         | PkReduce((_,_,e0),init,e)
         | PkScan((_,_,e0),init,e)->
             aux ~tailpos:false lenv env e0;
             aux ~tailpos:false lenv env init;
             aux ~tailpos:false lenv env e)
    | StackPrim _ ->
        assert false (* not yet introduced *)
    | Macro c ->
        (match c with
         | LazyOr(e1,e2) | LazyAnd(e1,e2) ->
             aux ~tailpos:false lenv env e1;
             aux ~tailpos lenv env e2
         | OCamlArrayMap(_,f,e1,e2) ->
             app ~tailpos f env lenv desc loc;
             aux ~tailpos:false lenv env e1;
             aux ~tailpos:false lenv env e2
         | OCamlArrayReduce(_,f,e1,e2) ->
             app ~tailpos f env lenv desc loc;
             aux ~tailpos:false lenv env e1;
             aux ~tailpos:false lenv env e2
         | OCamlArrayScan(_,f,e1,e2,e3) ->
             app ~tailpos f env lenv desc loc;
             aux ~tailpos:false lenv env e1;
             aux ~tailpos:false lenv env e2;
             aux ~tailpos:false lenv env e3
        )

in aux ~tailpos:true Env.empty Env.empty e

open Err

let check_tailrec ?(fmt=Format.std_formatter) ({x;decoration=loc;e} : circuit) : unit =
  try tailrec e with
  | Not_tailrec ((_,loc) as e) ->
    error x loc @@
        fun fmt () ->
        Format.fprintf fmt
          "The recursive call: %a should be in tail position."
          Pprint_ast.PP_MACLE.pp_exp e
