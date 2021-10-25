open Ast
open TMACLE
open Types

open Gensym

let mk_list_fold_left q ty tyr init el =
  let q' = gensym "aux" in
  let l = gensym "l" in
  let l2 = gensym "l2" in
  let x = gensym "x" in
  let acc = gensym "acc" in
  let acc2 = gensym "acc2" in
  let tyl = TCamlList ty in
  mk_letrec1 ~ty:tyr q' [(acc,tyr);(l,tyl)]
    (mk_if ~ty:tyr (mk_binop Eq (Var l) mk_empty_list) (Var acc) @@
     mk_let1 ~ty:tyr (x,ty) (mk_list_hd (Var l) ~ty_elem:ty) @@
     mk_let ~ty:tyr [ ((acc2,tyr),mk_app ~ty:tyr q [Var acc;Var x]);
                      ((l2,tyl),(mk_list_tl (Var l) ~ty_elem:ty)) ] @@
        (mk_app ~ty:tyr q' [Var acc2;Var l2])) @@
  mk_app ~ty:tyr  q' [init;el]

let mk_array_fold_left q ty tyr init earr =
  let q' = gensym "aux" in
  let y = gensym "arr" in
  let n = gensym "size" in
  let i = gensym "idx" in
  let x = gensym "x" in
  let acc = gensym "acc" in
  let acc2 = gensym "acc2" in

  mk_let1 ~ty:tyr (y,TCamlArray ty) earr @@
  mk_let1 ~ty:tyr (n,t_int) (mk_array_length ~ty (Var y)) @@
  mk_letrec1 ~ty:tyr q' [(acc,tyr);(i,t_int)] ( 
      mk_if   ~ty:tyr (mk_binop Ge (Var i) (Var n)) (Var acc) @@
      mk_let1 ~ty:tyr (x,ty) (mk_array_access (Var y) (Var i) ~ty_elem:ty) @@
      mk_let1 ~ty:tyr (acc2,tyr) (mk_app ~ty:tyr q [Var acc;Var x]) @@
      mk_app  ~ty:tyr q' [Var acc2; mk_binop Add (Var i) (mk_int 1)] 
    ) @@
    mk_app ~ty:tyr q' [init; mk_int 0]


let mk_array_map n q ty e =
  let q' = gensym "aux" in
  let y = gensym "arr" in
  let size = gensym "size" in
  let i = gensym "idx" in
  let elem = gensym "element" in

  mk_let1 ~ty:t_unit (y,TCamlArray ty) e @@
  mk_let1 ~ty:t_unit (size,t_int) (mk_array_length ~ty (Var y)) @@
  mk_letrec1 ~ty:t_unit q' [(i,t_int)]
  (mk_if ~ty:t_unit (mk_binop Ge (Var i) (Var size)) mk_unit @@
   mk_if ~ty:t_unit (mk_binop Ge (Var i) (mk_binop Sub (Var size) (mk_int n)))  
      (mk_let1 ~ty:t_unit (elem,ty) (mk_array_access (Var y) (Var i) ~ty_elem:ty) @@
       mk_seq ~ty:t_unit 
              (mk_array_assign (Var y) (Var i) (mk_app ~ty q [Var elem]) ~ty_elem:ty)
              (mk_app ~ty:t_unit q' [mk_binop Add (Var i) (mk_int 1)])) @@

        (let bs = List.init n (fun k -> 
                    ((elem^string_of_int k,ty),
                     (mk_array_access (Var y) 
                        (mk_binop Add (Var i) (mk_int k)) ~ty_elem:ty)))
        in
        mk_let_cascad ~ty:t_unit bs @@
        mk_let ~ty:t_unit 
          (List.init n (fun k -> (elem^string_of_int k,ty),
                           mk_app ~ty q [Var(elem^string_of_int k)])) @@
        mk_seqs ~ty:t_unit (List.init n (fun k ->
                      (mk_array_assign (Var y) (mk_binop Add (Var i) (mk_int k)) 
                          (Var (elem^string_of_int k)) ~ty_elem:ty))) @@
        (mk_app ~ty:t_unit q' [mk_binop Add (Var i) (mk_int n)])))
  (mk_app ~ty:t_unit q' [mk_int 0])
     


let rec expand e =
  match e with
  | (Var _ | Const _) -> e
  | Prim (c,es) ->
      Prim (c,List.map expand es)
  | If(e1,e2,e3,ty) ->
      let e1' = expand e1 in
      let e2' = expand e2 in
      let e3' = expand e3 in
      If(e1',e2',e3',ty)
  | Case(e1,ty,hs,e2,ty2) -> 
      let e1' = expand e1 in
      let hs' = List.map (fun (c,e) -> c,expand e) hs in
      let e2' = expand e2 in
      Case(e1',ty,hs',e2',ty2)
  | Let(bs,e,ty) ->
      let bs' = List.map (fun (x,e) -> (x,expand e)) bs in 
      let e' = expand e in
      mk_let ~ty bs' e'
  | LetFun((d,e1),e2) ->
      LetFun((d,expand e1),expand e2)
  | LetRec(bs,e,ty) ->
      LetRec(List.map (fun (d,e) -> (d,expand e)) bs,expand e,ty)
  | App(x,es,ty) ->
      App(x,List.map expand es,ty)
  | CamlPrim r -> 
    (match r with
    | RefAccess (e,t) -> 
        CamlPrim(RefAccess(expand e,t))
    | RefAssign{r;e;ty} -> 
        CamlPrim(RefAssign{r=expand r;
                           e=expand e;ty})
    | ArrayAccess{arr;idx;ty} ->
        CamlPrim(ArrayAccess{arr=expand arr;
                             idx=expand idx;ty})
    | ArrayAssign{arr;idx;e;ty} ->
        CamlPrim(ArrayAssign{arr=expand arr;
                             idx=expand idx;
                             e=expand e;ty})
    | ArrayLength (e,ty) -> 
        CamlPrim(ArrayLength(expand e,ty))
    | ListHd (e,ty) -> 
        CamlPrim(ListHd(expand e,ty))
    | ListTl (e,ty) -> 
        CamlPrim(ListTl(expand e,ty))
    | ListFoldLeft(q,ty,tyr,init,e) ->
        expand @@
        mk_list_fold_left q ty tyr init e
    | ArrayFoldLeft(q,ty,tyr,init,e) ->
        expand @@
        mk_array_fold_left q ty tyr init e
    | ArrayMapBy(n,q,ty,e) -> 
        expand @@
        mk_array_map n q ty e
    )


let expand_circuit (c : TMACLE.circuit) : TMACLE.circuit = 
  {c with e = expand c.e}
