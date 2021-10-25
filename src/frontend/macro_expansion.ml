open Ast
open TMACLE
open Types

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
        App(q',[init;el],tyr),tyr)

let mk_array_fold_left q ty tyr init earr =
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
                    Let([((x,ty),CamlPrim(ArrayAccess {arr=Var y;idx=Var i;ty}))],
                        Let([((acc2,tyr),App(q,[Var acc;Var x],tyr))],
                            App(q',[Var acc2;
                                    Prim(Atom.Binop Add,
                                         [Var i;Const (Atom.mk_int 1)])],tyr),tyr),tyr),tyr))],
               App(q',[init;Const (Atom.mk_int 0)],tyr),tyr),tyr),tyr)

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
              App(q',[Const (Int 0)],t_unit),t_unit),t_unit),t_unit)


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
      let e' = expand e in (* moins les xi *)
      mk_let bs' e' ty
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
