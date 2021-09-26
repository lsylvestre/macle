open Ast
open TMACLE
open Types

(** systematically inline function calls.
    tail-recursive functions are translated into local loops.
    
    Non tail-recursive functions are not supported : 
    this silently generates incorrect code (to be improved !) *)

(* Inlining specializes the higher order functions, possibly recursive.
   TODO : fix mutual recursive HOFs *)

(** Inlining assume that each identifier binded by Let/LetFun/LetRec is unique:
    a renaming of identifiers must be perform on the source code beforehand. *)

let rec occur x e =
  match e with
  | Var x' ->
      x = x'
  | Const _ ->
      false
  | Prim (_,es) ->
      List.exists (occur x) es
  | If(e1,e2,e3,_) -> 
      occur x e1 || occur x e2 || occur x e3
  | Case(e1,_,hs,e2,_) -> 
      occur x e1 ||
      List.exists (fun (_,e) -> occur x e) hs ||
      occur x e2
  | Let(bs,e,_) -> 
      List.exists (fun (_,e) -> occur x e) bs ||
      (List.for_all (fun ((x',_),_) -> x <> x') bs && occur x e)
  | LetFun(((q,xs),e1),e2) -> 
      (List.for_all (fun (y,_) -> y <> x) xs && occur x e1) ||
      (x <> q && occur x e2)
  | LetRec(bs,e) -> 
      (List.for_all (fun ((x',xs),_) ->
                       x <> x' && List.for_all (fun (y,_) -> x <> y) xs) bs) && 
      (List.exists (fun ((_,_),e) -> occur x e) bs || occur x e)
  | App(x',es,_) -> 
      x' = x || List.exists (occur x) es
  | CamlPrim c -> 
      (match c with 
       | ArrayAccess{arr;idx} ->
           occur x arr || occur x idx
       | (RefAccess (e,_) | ArrayLength (e,_) | ListHd (e,_) | ListTl (e,_)) -> 
           occur x e
       | RefAssign{r;e} ->
           occur x r || occur x e
       | ArrayAssign{arr;idx;e} ->
           occur x arr || occur x idx || occur x e
       | (ListFoldLeft(q,_,_,init,e) 
          | ArrayFoldLeft(q,_,_,init,e)) ->
           x = q || occur x init || occur x e
       | ArrayMapBy(_,q,_,e) ->
           x = q || occur x e)


let env_rec_extend ?(recflag=false) bs env =
  let rframe = ref (if recflag then bs else []) in
  List.map (fun ((x,xs),e) -> (x,(xs,e,rframe))) bs @ env    


let substitution env e =
  let rec aux e = match e with
  | Var x -> (match List.assoc_opt x env with None -> e | Some e' -> e')
  | Const _ -> 
      e
  | Prim (p,es) ->
      Prim (p, List.map aux es)
  | If(e1,e2,e3,ty) -> 
      If(aux e1,aux e2,aux e3,ty)
  | Case(e1,ty,hs,e2,ty2) -> 
      Case(aux e1,ty,Misc.map_snd aux hs,aux e2,ty2)
  | Let(bs,e,ty) -> 
      Let(Misc.map_snd aux bs,aux e,ty)
  | LetFun((qxs,e1),e2) -> 
      LetFun((qxs,aux e1),aux e2)
  | LetRec(bs,e) -> 
      LetRec(Misc.map_snd aux bs,aux e)
  | App(x,es,ty) -> 
      let es' = List.map aux es in
      (match List.assoc_opt x env with 
       | None -> App(x,es',ty)
       | Some (Var q) -> App(q,List.map aux es,ty)
       | _ -> assert false)
  | CamlPrim c ->
      CamlPrim 
      (match c with 
       | ArrayAccess{arr;idx;ty} ->
           ArrayAccess{arr=aux arr;idx=aux idx;ty}
       | RefAccess (e,ty) ->
           RefAccess (aux e,ty)
       | ArrayLength (e,ty) ->
           ArrayLength (aux e,ty) 
       | ListHd (e,ty) -> 
           ListHd (aux e,ty)
       | ListTl (e,ty) -> 
           ListTl (aux e,ty)
       | RefAssign{r;e;ty} ->
            RefAssign{r=aux r;e= aux e;ty}
       | ArrayAssign{arr;idx;e;ty} ->
           ArrayAssign{arr= aux arr;idx= aux idx;e= aux e;ty}
       | ListFoldLeft(q,ty1,ty2,init,e) ->
           ListFoldLeft(q,ty1,ty2,aux init,aux e) 
       | ArrayFoldLeft(q,ty1,ty2,init,e) ->
           ArrayFoldLeft(q,ty1,ty2,aux init,aux e)
       | ArrayMapBy(n,q,ty,e) ->
           ArrayMapBy(n,q,ty,aux e))
in aux e


let is_fun_type = function
| Types.TFun _ -> true
| _ -> false

let specialize_list xs es e =
  let bs = (List.combine xs es) in
  let env,bs = List.partition_map (fun ((x,ty) as xty,e) -> 
      if is_fun_type ty then Left(x,e) else Right (xty,e)) @@ bs in
  let xs,es = List.split bs in
  (* *********************************** *)
  (* 
  List.iter (fun (x,e') -> Format.fprintf Format.err_formatter "%s / %a IN %a\n#########" x Pprint_ast.PP_TMACLE.pp_exp e'  Pprint_ast.PP_TMACLE.pp_exp e;) env;
  Format.fprintf Format.err_formatter "[[[[%a]]]]]]\n"  Pprint_ast.PP_TMACLE.pp_exp (substitution env e);
  *)
  (* *********************************** *)
  xs,es,env,substitution env e


let fetch x env =
  match List.assoc_opt x env with
  | None ->
      (* [x] doit être défini dans [env] puisque le programme est supposé bien typé *)
    Printf.printf "** %s" x; 
    assert false
  | Some (xs,e,bs) ->
      (xs,e,bs)

let mk_list_fold_left q ty tyr init el =
  let q' = Gensym.gensym "aux" in
  let l = Gensym.gensym "l" in
  let l2 = Gensym.gensym "l2" in
  let x = Gensym.gensym "x" in
  let acc = Gensym.gensym "acc" in
  let acc2 = Gensym.gensym "acc2" in
  let tyl = TCamlList ty in
  LetRec([((q',[(acc,tyr);(l,tyl)]), 
         If(Prim(Atom.Binop Eq,[Var l;Const EmptyList]),Var acc,
            Let([((x,ty),CamlPrim(ListHd (Var l,ty)))],
                Let([((acc2,tyr),App(q,[Var acc;Var x],tyr));
                     ((l2,tyl),CamlPrim(ListTl (Var l,ty)))],
                 App(q',[Var acc2;Var l2],tyr),tyr),tyr),tyr))],
        App(q',[init;el],tyr))

let mk_array_fold_left q ty tyr init earr = (* résultat inatendu !? *)
  let q' = Gensym.gensym "aux" in
  let y = Gensym.gensym "arr" in
  let n = Gensym.gensym "size" in
  let i = Gensym.gensym "idx" in
  let x = Gensym.gensym "x" in
  let acc = Gensym.gensym "acc" in
  let acc2 = Gensym.gensym "acc2" in
  Let([((y,TCamlArray ty),earr)],
       Let([(n,TConst TInt),CamlPrim (ArrayLength (Var y,ty))],
       LetRec([((q',[(acc,tyr);(i,TConst TInt)]), 
                 If(Prim(Atom.Binop Ge,[Var i;Var n]),Var acc,
                    Let([((x,ty),CamlPrim(ArrayAccess {arr=Var y;idx=Var i;ty}));
                        ((acc2,tyr),App(q,[Var acc;Var x],tyr))],
                            App(q',[Var acc2;
                                    Prim(Atom.Binop Add,
                                         [Var i;Const (Atom.mk_int 1)])],tyr),tyr),tyr))],
               App(q',[init;Const (Atom.mk_int 0)],tyr)),tyr),tyr)

let let_set bs ty e = 
  List.fold_right (fun b e -> mk_let [b] e ty) bs e

let let_par bs ty e = 
  mk_let bs e ty 

let add a b = Prim (Atom.Binop Add,[a;b])

let t_unit = TConst TUnit

let mk_array_map n q ty e = (* résultat inatendu ! *)
  let q' = Gensym.gensym "aux" in
  let y = Gensym.gensym "arr" in
  let size = Gensym.gensym "size" in
  let i = Gensym.gensym "idx" in
  let elem = Gensym.gensym "element" in
  Let([((y,TCamlArray ty),e)],
     Let([(size,TConst TInt),CamlPrim (ArrayLength (Var y,ty))],
       LetRec([((q',[(i,TConst TInt)]), 
                 If(Prim(Atom.Binop Ge,[Var i;Var size]),(* Prim(Atom.Binop Gt,[Var i;Prim (Binop Sub,[Var size;Const (Int n)])]), *)
                    Const Unit,
                    (let bs = List.init n (fun k -> 
                                ((elem^string_of_int k,ty),
                                 CamlPrim(ArrayAccess {arr=Var y;
                                                       idx=add (Var i) (Const (Int k));
                                                       ty})))
                    in
                    let bs2 = List.init n (fun k -> (elem^string_of_int k,ty),
                                                        App(q,[Var(elem^string_of_int k)],ty)) in
                    let bs3 =  List.init n (fun k ->
                                 (("ignore",TConst TUnit),
                                  CamlPrim(ArrayAssign {arr=Var y;
                                                        idx=add (Var i) (Const (Int k));
                                                        e=Var (elem^string_of_int k);
                                                        ty}))) in
                    let_set bs t_unit @@
                    let_par bs2 t_unit @@
                    let_set bs3 t_unit @@
                    App(q',[add (Var i) (Const (Int n))],t_unit)),t_unit))],
              App(q',[Const (Int 0)],t_unit)),t_unit),t_unit)


let rec inline rec_env env e =
  (* *********************************** *)
  (*
  let open Format in
  List.iter (fun (x,_) -> fprintf err_formatter "%s," x) env;
   fprintf err_formatter "\n----\n";
  Pprint_ast.PP_TMACLE.pp_exp Format.err_formatter e;Format.(fprintf err_formatter "\n==============\n");
  *)
  (* *********************************** *)
  match e with
  | (Var _ | Const _) -> e
  | Prim (c,es) ->
      Prim (c,List.map (inline rec_env env) es)
  | If(e1,e2,e3,ty) ->
      let e1' = inline rec_env env e1 in
      let e2' = inline rec_env env e2 in
      let e3' = inline rec_env env e3 in
      If(e1',e2',e3',ty)
  | Case(e1,ty,hs,e2,ty2) -> 
      let e1' = inline rec_env env e1 in
      let hs' = List.map (fun (c,e) -> c,inline rec_env env e) hs in
      let e2' = inline rec_env env e2 in
      Case(e1',ty,hs',e2',ty2)
  | Let(bs,e,ty) ->
      let bs' = List.map (fun (x,e) -> (x,inline rec_env env e)) bs in 
      let e' = inline rec_env env e in (* moins les xi *)
      mk_let bs' e' ty
  | LetFun(b,e) ->
      (* [b] est "poussé" dans l'environnement *)
      let env' = env_rec_extend [b] env in
      inline rec_env env' e
  | LetRec(bs,e) ->
      (* les [bs] sont "poussés" dans l'environnement *)
      let env' = env_rec_extend ~recflag:true bs env in
      inline rec_env env' e
  | App(x,es,ty) ->
      let (xs,e,rbs) = fetch x env in
      let bs = !rbs in
      (*  List.filter_map (fun ((_,ty),e) -> 
          if is_fun_type ty then None else Some (inline rec_env env e)) 
          (List.combine xs es) in *)
      
      let xs2,es2,env0,e0 = specialize_list xs es e in
      let es' = List.map (inline rec_env env) es2 in
      if List.mem x rec_env then App(x,es',ty) else
      if List.exists (fun (_,e) -> occur x e) bs (* fonction récursive *)
      then let bs' = 
             List.map (fun ((y,xs3),e) ->
                 let rec_env' = List.map (fun ((x,_),_) -> x) bs @ rec_env in
                 let e' = if x = y then inline rec_env' env e0 else inline rec_env' env e in
                 (* Pprint_ast.PP_TMACLE.pp_exp Format.err_formatter e';Format.(fprintf err_formatter "\n%s~~~~~~\n" y); *)
                 let xs' = if x = y then xs2 else List.filter (fun (_,ty) -> not @@ is_fun_type ty) xs3 in
                 (y,xs'),e') bs
           in
           let e1 = LetRec(bs',App(x,es',ty)) in
           (* Pprint_ast.PP_TMACLE.pp_exp Format.err_formatter e1;*)
           e1
      else let bs' = List.combine xs2 es' in
           let e' = inline rec_env env e0 in
           (* let bs'' = List.filter (fun ((x,ty),_) -> not (is_fun_type ty)) bs' in *)
           mk_let bs' e' ty
  | CamlPrim r -> 
    (match r with
    | RefAccess (e,t) -> 
        CamlPrim(RefAccess(inline rec_env env e,t))
    | RefAssign{r;e;ty} -> 
        CamlPrim(RefAssign{r=inline rec_env env r;
                           e=inline rec_env env e;ty})
    | ArrayAccess{arr;idx;ty} ->
        CamlPrim(ArrayAccess{arr=inline rec_env env arr;
                             idx=inline rec_env env idx;ty})
    | ArrayAssign{arr;idx;e;ty} ->
        CamlPrim(ArrayAssign{arr=inline rec_env env arr;
                             idx=inline rec_env env idx;
                             e=inline rec_env env e;ty})
    | ArrayLength (e,ty) -> 
        CamlPrim(ArrayLength(inline rec_env env e,ty))
    | ListHd (e,ty) -> 
        CamlPrim(ListHd(inline rec_env env e,ty))
    | ListTl (e,ty) -> 
        CamlPrim(ListTl(inline rec_env env e,ty))
    | ListFoldLeft(q,ty,tyr,init,e) ->
        inline rec_env env @@
        mk_list_fold_left q ty tyr init e
    | ArrayFoldLeft(q,ty,tyr,init,e) ->
        inline rec_env env @@
        mk_array_fold_left q ty tyr init e
    | ArrayMapBy(n,q,ty,e) -> 
        inline rec_env env @@
        mk_array_map n q ty e
    )


let inline_circuit (c : TMACLE.circuit) : TMACLE.circuit = 
  {c with e = inline [] [] c.e}
