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
       | ArrayLength e ->
           ArrayLength (aux e) 
       | ListHd e -> 
           ListHd (aux e)
       | ListTl e -> 
           ListTl (aux e)
       | RefAssign{r;e} ->
            RefAssign{ r = aux r ; e = aux e }
       | ArrayAssign{arr;idx;e} ->
           ArrayAssign{ arr = aux arr ; idx = aux idx ; e = aux e }
       | (ListFoldLeft _ | ArrayFoldLeft _ | ArrayMapBy _) -> 
           assert false (* already expanded *) )
in aux e


let is_fun_type = function
| Types.TFun _ -> true
| _ -> false

let specialize_list xs es e =
  let bs = List.combine xs es in
  let env,bs = 
    Misc.partition_map (fun (((x,ty),e) as b) -> 
                         if is_fun_type ty then Misc.Left(x,fst e) 
                                           else Misc.Right b) bs in
  bs,substitution env e


let fetch x env =
  match List.assoc_opt x env with
  | None ->
      (* [x] doit être défini dans [env] 
         puisque le programme est supposé bien typé *)
    Printf.printf "** %s" x; 
    assert false
  | Some b ->
      b

let rec inline rec_env env (desc,ty) =
  match desc with
  | (Var _ | Const _) -> desc,ty
  | Unop(p,e) ->
      Unop(p,inline rec_env env e),ty
  | Binop(p,e1,e2) ->
      Binop(p,inline rec_env env e1, inline rec_env env e2),ty
  | If(e1,e2,e3) ->
      let e1' = inline rec_env env e1 in
      let e2' = inline rec_env env e2 in
      let e3' = inline rec_env env e3 in
      mk_if e1' e2' e3'
  | Let(bs,e) ->
      let bs' = List.map (fun (x,e) -> (x,inline rec_env env e)) bs in 
      let e' = inline rec_env env e in (* moins les xi *)
      mk_let bs' e'
  | LetFun(((x,args),e1),e2) ->
      (* [b] est "poussé" dans l'environnement *)
      (* renomme [x] au passage *) 
      let x' = Gensym.gensym x in
      let theta = [(x,Var x')] in
      let b' = ((x',args),substitution theta e1) in
      let env' = env_extend [b'] env in
      inline rec_env env' (substitution theta e2)
  | LetRec(bs,e) ->
      (* les [bs] sont "poussés" dans l'environnement *)
      (* renomme au passage les bs : c'est nécessaire en cas d'application
         d'une fonction [f] à une expression qui contient [f]. 
       *)
      let bs' = List.map (fun ((x,args),e) -> ((Gensym.gensym x,args),e)) bs in
      let env0 = List.map2 (fun ((x,_),e) ((x',_),_) -> (x,Var x')) bs bs' in
      let e = substitution env0 e in 
      let bs'' = List.map (fun ((x,args),e) -> ((x,args),substitution env0 e)) bs' in
      let env' = env_extend bs'' env in
      inline rec_env env' e
  | App(x,es) ->
      let es' = List.map (inline rec_env env) es in
      let (xs,e,bs) = fetch x env in
      if List.mem x rec_env then mk_app ~ty x es' (* déjà traité *) 
      else if List.exists (fun (_,e) -> occur x e) bs (* fonction récursive *)
           then let bs' = 
               List.map (fun ((y,xs'),e) ->
                 let rec_env' = List.map (fun ((x,_),_) -> x) bs @ rec_env in
                 (y,xs'),inline rec_env' env e) bs
             in
             mk_letrec bs' (mk_app ~ty x es')
      else
        let bs',e0 = specialize_list xs es' e in
        let e' = inline rec_env env e0 in
        mk_let bs' e'
  | Match(e,cases) ->
      let e' = inline rec_env env e in
      let cases' = List.map (fun (c,xs,e) -> (c,xs,inline rec_env env e)) cases in 
      Match(e',cases'),ty
  | Raise _ -> 
      desc,ty
  | CamlPrim r -> 
    let c = 
      match r with
      | RefAccess e -> 
          RefAccess(inline rec_env env e)
      | RefAssign{r;e} -> 
          RefAssign{ r = inline rec_env env r ; e = inline rec_env env e }
      | ArrayAccess{arr;idx} ->
          ArrayAccess{ arr = inline rec_env env arr ;
                       idx = inline rec_env env idx }
      | ArrayAssign{arr;idx;e} ->
          ArrayAssign{ arr = inline rec_env env arr ;
                       idx = inline rec_env env idx ;
                       e = inline rec_env env e}
      | ArrayLength e -> 
          ArrayLength(inline rec_env env e)
      | ListHd e -> 
          ListHd(inline rec_env env e)
      | ListTl e -> 
          ListTl(inline rec_env env e)
      | (ListFoldLeft _ | ArrayFoldLeft _ | ArrayMapBy _) -> 
          assert false (* already expanded *) 
    in (CamlPrim c),ty


let inline_circuit (c : TMACLE.circuit) : TMACLE.circuit = 
  {c with e = inline [] [] c.e}
