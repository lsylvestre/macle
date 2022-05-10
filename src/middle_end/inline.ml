open Ast
open TMACLE
open Types

open Occur
(** systematically inline function calls.
    tail-recursive functions are translated into local loops.

    Non tail-recursive functions are not supported :
    this silently generates incorrect code (to be improved !) *)

(* higher order functions that are not recursive are specialized *)

(** Inlining assume that each identifier binded by Let/LetFun/LetRec is unique:
    a renaming of identifiers must be perform on the source code beforehand. *)

let env_extend bs env =
  List.map (fun ((x,xs),e) -> (x,(xs,e,bs))) bs @ env


let substitution env (e:exp) =
  let rec aux (desc,ty) =
    (fun desc -> desc,ty) @@
    match desc with
    | Var x ->
      (match List.assoc_opt x env with
       | None -> desc
       | Some desc' -> desc')
    | Const _ ->
        desc
    | Unop(p,e) ->
        Unop(p,aux e)
    | Binop(p,e1,e2) ->
        Binop(p,aux e1,aux e2)
    | If(e1,e2,e3) ->
        If(aux e1,aux e2,aux e3)
    | Let(bs,e) ->
        Let(Misc.map_snd aux bs,aux e)
    | LetFun((qxs,e1),e2) ->
        LetFun((qxs,aux e1),aux e2)
    | LetRec(bs,e) ->
        LetRec(Misc.map_snd aux bs,aux e)
    | App(x,es) ->
        let es' = List.map aux es in
        (match List.assoc_opt x env with
         | None -> App(x,es')
         | Some Var q -> App(q,List.map aux es)
         | _ -> assert false)
    | Match(e,cases) ->
        let e' = aux e in
        let cases' =
          List.map (fun (c,xs,e) ->
              assert (List.for_all (function
                  | Some x,_ -> not (List.mem_assoc x env)
                  | None,_ -> true) xs);
              (c,xs,aux e)) cases in
        Match(e',cases')
    | Raise _ ->
        desc
    | CamlPrim c ->
        CamlPrim
          (match c with
           | ArrayAccess{arr;idx} ->
               ArrayAccess{ arr = aux arr ; idx = aux idx }
           | RefAccess e ->
               RefAccess (aux e)
           | Ref e -> 
               Ref (aux e)
           | ArrayLength e ->
               ArrayLength (aux e)
           | RefAssign{r;e} ->
               RefAssign{ r = aux r ; e = aux e }
           | ArrayAssign{arr;idx;e} ->
               ArrayAssign{ arr = aux arr ; idx = aux idx ; e = aux e }
           | ArrayMake{size;e} -> 
               let size = aux size in
               let e = aux e in
               ArrayMake{size;e})
    | PacketPrim c ->
      PacketPrim
        (match c with
         | PkMake es ->
             PkMake (List.map aux es)
         | PkGet(e,idx) ->
             PkGet (aux e,aux idx)
         | PkSet(x,idx,e) ->
             PkSet(x,aux idx,aux e)
         | ToPacket(e,idx,n) ->
             ToPacket(aux e,aux idx,n)
         | OfPacket(e1,e2,idx,n) ->
             OfPacket(aux e1,aux e2,aux idx,n)
         | PkMap((xs,e),es) ->
             PkMap((xs,aux e),List.map aux es)
         | PkReduce((x,y,e0),init,e) ->
             PkReduce((x,y,aux e0),aux init,aux e)
         | PkScan((x,y,e0),init,e) ->
             PkScan((x,y,aux e0),aux init,aux e))
    | Macro _ ->
        assert false
    | StackPrim _ ->
      assert false (* not yet introduced *)
in aux e

(** [is_fun_type ty] is [true] if [ty] is a functionnal type, else [false]. *)
let is_fun_type = function
| Types.TFun _ -> true
| _ -> false

(** Given a list [xs] of arguments [x1], ... [xn],
    a list [es] of expression [e1], ... [en],
    and an expression [e],
    replace each [xi] by [ei] in [e] if the type of [x_i] is functionnal. *)
let specialize_list xs es e =
  let bs = List.combine xs es in
  let select (((x,ty),e) as b) =
    if is_fun_type ty then Misc.Left(x,fst e)
    else Misc.Right b
  in
  let env,bs = Misc.partition_map select bs in
  bs,substitution env e

(** [fetch x env] lookups the object associated to [x] in [env].
    Assume that [x] is mapped in [env]. *)
let fetch x env =
  match List.assoc_opt x env with
  | None ->
      (* [x] doit être défini dans [env]
         puisque le programme est supposé bien typé *)
    Printf.printf "** %s" x;
    assert false
  | Some b ->
      b

(** [inline rec_call env e] inline all applications [f e1 ... en]
    if [f] not in [rec_call]
 *)

let rec inline (rec_call: name list) env (e:exp) : exp =
  let (desc,ty) = e in
  match desc with
  | (Var _ | Const _) -> desc,ty
  | Unop(p,e) ->
      Unop(p,inline rec_call env e),ty
  | Binop(p,e1,e2) ->
      Binop(p,inline rec_call env e1, inline rec_call env e2),ty
  | If(e1,e2,e3) ->
      let e1' = inline rec_call env e1 in
      let e2' = inline rec_call env e2 in
      let e3' = inline rec_call env e3 in
      mk_if e1' e2' e3'
  | Let(bs,e) ->
      let bs' = List.map (fun (x,e) -> (x,inline rec_call env e)) bs in
      let e' = inline rec_call env e in (* moins les xi *)
      mk_let bs' e'
  | LetFun(((x,args),e1),e2) ->
      (* push [b] into the environment *)
      (* rename [x] *)
      let x' = Gensym.gensym x in
      let theta = [(x,Var x')] in
      let b' = ((x',args),substitution theta e1) in
      let env' = env_extend [b'] env in
      inline rec_call env' (substitution theta e2)
  | LetRec(bs,e) ->
      (* push [bs] into the environment *)
      (* rename the [bs] : it is needed in case of the application of a function [f] to an expression that contains [f]. *)
      let bs' = List.map (fun ((x,args),e) -> ((Gensym.gensym x,args),e)) bs in
      let env0 = List.map2 (fun ((x,_),e) ((x',_),_) -> (x,Var x')) bs bs' in
      let e = substitution env0 e in
      let bs'' = List.map (fun ((x,args),e) -> ((x,args),substitution env0 e)) bs' in
      let env' = env_extend bs'' env in
      inline rec_call env' e
  | App(x,es) ->
      let es' = List.map (inline rec_call env) es in
      let (xs,e,bs) = fetch x env in
      if List.mem x rec_call
      then (* have already dealt with *)
        mk_app ~ty x es' else
      if List.exists (fun (_,e) -> occur x e) bs (* recursive function *)
      then let bs' =
             List.map (fun ((y,xs'),e) ->
                 let rec_call' = List.map (fun ((x,_),_) -> x) bs @ rec_call in
                 (y,xs'),inline rec_call' env e) bs
        in
        mk_letrec bs' (mk_app ~ty x es')
      else
        let bs',e0 = specialize_list xs es' e in
        let e' = inline rec_call env e0 in
        mk_let bs' e'
  | Match(e,cases) ->
      let inline_case (c,xs,e) =
        (c,xs,inline rec_call env e)
      in
      let e' = inline rec_call env e in
      let cases' = List.map inline_case cases in
      Match(e',cases'),ty
  | Raise _ ->
      desc,ty
  | CamlPrim r ->
      let c =
        match r with
        | RefAccess e ->
            RefAccess(inline rec_call env e)
        | RefAssign{r;e} ->
            let r = inline rec_call env r in
            let e = inline rec_call env e in
            RefAssign{ r ; e }
        | Ref e ->
            Ref (inline rec_call env e)
        | ArrayAccess{arr;idx} ->
            let arr = inline rec_call env arr in
            let idx = inline rec_call env idx in
            ArrayAccess{ arr ; idx }
        | ArrayAssign{arr;idx;e} ->
            let arr = inline rec_call env arr in
            let idx = inline rec_call env idx in
            let e = inline rec_call env e in
            ArrayAssign{ arr ; idx ; e }
        | ArrayMake{size;e} ->
            let size = inline rec_call env size in
            let e = inline rec_call env e in
            ArrayMake{size;e}
        | ArrayLength e ->
            let e' = inline rec_call env e in
            ArrayLength(e')
      in
      (CamlPrim c),ty
  | PacketPrim c ->
      PacketPrim
        (match c with
         | PkMake es ->
             PkMake (List.map (inline rec_call env) es)
         | PkGet(e,idx) ->
             let e = inline rec_call env e in
             let idx = inline rec_call env idx in
             PkGet(e, idx)
         | PkSet(x,e,idx) ->
             let idx = inline rec_call env idx in
             let e = inline rec_call env e in
             PkSet(x,e,idx)
         | ToPacket(e,idx,n) ->
             let e' = inline rec_call env e in
             let idx' = inline rec_call env idx in
             ToPacket(e',idx',n)
         | OfPacket(e1,e2,idx,n) ->
             let e1' = inline rec_call env e1 in
             let e2' = inline rec_call env e2 in
             let idx' = inline rec_call env idx in
             OfPacket(e1',e2',idx',n)
         | PkMap((xs,e),es) ->
             let e' = inline rec_call env e in
             let es' = List.map (inline rec_call env) es in
             PkMap((xs,e'),es')
         | PkReduce((x,y,e0),init,e) ->
             let e0' = inline rec_call env e0 in
             let init' = inline rec_call env init in
             let e' = inline rec_call env e in
             PkReduce((x,y,e0'),init',e')
         | PkScan((x,y,e0),init,e) ->
             let e0' = inline rec_call env e0 in
             let init' = inline rec_call env init in
             let e' = inline rec_call env e in
             PkScan((x,y,e0'),init',e')),ty
  | Macro _ ->
      assert false (* already expanded *)
  | StackPrim _ ->
      assert false (* not yet introduced *)

let inline_circuit (c : TMACLE.circuit) : TMACLE.circuit =
  {c with e = inline [] [] c.e}
