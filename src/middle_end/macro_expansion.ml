open Ast
open TMACLE
open Types

open Gensym

(* expand :
   - [or e1 e2] into [if e1 then true else e2]
   - [and e1 e2] into [if e1 then e2 else false]
   - array/list combinators
   - wrap array_get and array_set
     into a safe error handling to manage index out of bounds 

*)

let mk_array_fold_left q init earr =
  let tarr_ty = ty_of earr in
  let ty = array_ty tarr_ty in
  let tyr = ty_of init in
  let q' = gensym "aux" in
  let y = gensym "arr" in
  let n = gensym "size" in
  let i = gensym "idx" in
  let x = gensym "x" in
  let acc = gensym "acc" in
  let acc2 = gensym "acc2" in

  let var_y = Var y,tarr_ty in
  mk_let1 (y,tarr_ty) earr @@
  mk_let1 (n,t_int) (mk_array_length var_y) @@
  mk_letrec1 q' [(acc,tyr);(i,t_int)] ( 
      mk_if  (mk_binop ~ty:t_bool Ge (Var i,t_int) (Var n,t_int)) (Var acc,tyr) @@
      mk_let1 (x,ty) (mk_array_access var_y (Var i,t_int)) @@
      mk_let1 (acc2,tyr) (mk_app ~ty:tyr q [Var acc,tyr;Var x,ty]) @@
      mk_app ~ty:tyr q' [Var acc2,tyr; mk_binop ~ty:t_int Add (Var i,t_int) (mk_int 1)] 
    ) @@
    mk_app ~ty:tyr q' [init; mk_int 0]


let mk_array_map n q e =
  let ty = ty_of e in
  let ty_elem = array_ty ty in
  let q' = gensym "aux" in
  let y = gensym "arr" in
  let size = gensym "size" in
  let i = gensym "idx" in
  let elem = gensym "element" in

  let var_y = (Var y,ty) in
  let var_i = Var i,t_int in
  let var_size = Var size,t_int in
  mk_let1 (y,ty) e @@
  mk_let1 (size,t_int) (mk_array_length var_y) @@
  mk_letrec1 q' [(i,t_int)]
  (mk_if (mk_binop ~ty:t_bool Ge var_i var_size) mk_unit @@
   mk_if (mk_binop ~ty:t_bool Gt var_i (mk_binop ~ty:t_int Sub var_size (mk_int n)))  
      (mk_let1  (elem,ty_elem) (mk_array_access var_y var_i) @@
       mk_seq
              (mk_array_assign var_y var_i (mk_app ~ty:ty_elem q [Var elem,ty_elem]))
              (mk_app ~ty:t_unit q' [mk_binop ~ty:t_int Add var_i (mk_int 1)])) @@

        (let bs = List.init n (fun k -> 
                    ((elem^string_of_int k,ty_elem),
                     (mk_array_access var_y 
                        (mk_binop ~ty:t_int Add var_i (mk_int k)))))
        in
        mk_let_cascad bs @@
        mk_let 
          (List.init n (fun k -> (elem^string_of_int k,ty_elem),
                           mk_app ~ty q [Var(elem^string_of_int k),ty_elem])) @@
        mk_seqs (List.init n (fun k ->
                      (mk_array_assign var_y (mk_binop ~ty:t_int Add var_i (mk_int k)) 
                          (Var (elem^string_of_int k),ty_elem)))) @@
        (mk_app ~ty:t_unit q' [mk_binop ~ty:t_int Add var_i (mk_int n)])))
  (mk_app ~ty:t_unit q' [mk_int 0])
     


let rec expand ~safe ((desc,ty) as e) =
  match desc with
  | (Var _ | Const _) -> e
  | Unop(p,e) ->
      Unop(p,expand ~safe e),ty
  | Binop(Or,e1,e2) ->
     (* lazy [or] *)
     mk_if e1 (mk_bool true) e2
  | Binop(And,e1,e2) ->
     (* lazy [and] *)
     mk_if e1 e2 (mk_bool false)
  | Binop(p,e1,e2) ->
      Binop(p,expand ~safe e1, expand ~safe e2),ty
  | If(e1,e2,e3) ->
      let e1' = expand ~safe e1 in
      let e2' = expand ~safe e2 in
      let e3' = expand ~safe e3 in
      mk_if e1' e2' e3'
  | Let(bs,e) ->
      let bs' = List.map (fun (x,e) -> (x,expand ~safe e)) bs in 
      let e' = expand ~safe e in
      mk_let bs' e'
  | LetFun((d,e1),e2) ->
      LetFun((d,expand ~safe e1),expand ~safe e2),ty
  | LetRec(bs,e) ->
      mk_letrec (List.map (fun (d,e) -> (d,expand ~safe e)) bs) (expand ~safe e)
  | App(x,es) ->
      mk_app ~ty x (List.map (expand ~safe) es)
  | Match(e,cases) -> 
      let cases' = List.map (fun (c,xs,e) -> c,xs,expand ~safe e) cases in
      Match(expand ~safe e,cases'),ty
  | Raise _ -> 
      e
  | CamlPrim r -> 
    (match r with
    | RefAccess e -> 
        CamlPrim(RefAccess(expand ~safe e)),ty
    | RefAssign{r;e} -> 
        CamlPrim(RefAssign{
                  r = expand ~safe r ;
                  e = expand ~safe e
                }),ty
    | ArrayAccess{arr;idx} ->
        if not safe then 
          CamlPrim (
            ArrayAccess {
              arr = expand ~safe arr; 
              idx = expand ~safe idx
            }),ty
        else
        let x_arr = gensym "x" in
        let y_idx = gensym "y" in
        let z = gensym "z" in
        let err = Raise (Exception_Invalid_arg "Index out of bounds"),ty in
        mk_let [(x_arr,ty_of arr),expand ~safe arr;
                (y_idx,t_int),expand ~safe idx] @@
        mk_if (mk_binop ~ty:t_bool Lt (Var y_idx,t_int) (mk_int 0)) err @@
        mk_let1 (z,t_int) (mk_array_length (Var x_arr, ty_of arr)) @@
        mk_if (mk_binop ~ty:t_bool Ge (Var y_idx,t_int) (Var z,t_int)) err 
        @@
        (CamlPrim(ArrayAccess{ 
                  arr = (Var x_arr,ty_of arr) ;
                  idx = (Var y_idx,t_int)
                }),ty)
    | ArrayAssign{arr;idx;e} ->
        if not safe then 
          CamlPrim (
            ArrayAssign {
              arr = expand ~safe arr; 
              idx = expand ~safe idx;
              e = expand ~safe e
            }),ty
        else
        let x_arr = gensym "x" in
        let y_idx = gensym "y" in
        let z = gensym "z" in
        let err = Raise (Exception_Invalid_arg "Index out of bounds"),ty in
        mk_let [(x_arr,ty_of arr),expand ~safe arr;
                (y_idx,t_int),expand ~safe idx] @@
        mk_if (mk_binop ~ty:t_bool Lt (Var y_idx,t_int) (mk_int 0)) err @@
        mk_let1 (z,t_int) (mk_array_length (Var x_arr, ty_of arr)) @@
        mk_if (mk_binop ~ty:t_bool Ge (Var y_idx,t_int) (Var z,t_int)) err
        @@
        (CamlPrim(ArrayAssign{ 
                  arr = (Var x_arr,ty_of arr) ;
                  idx = (Var y_idx,t_int) ;
                  e = expand ~safe e
                }),ty)
    | ArrayLength e -> 
        CamlPrim(ArrayLength(expand ~safe e)),ty
    | ArrayFoldLeft(q,init,e) ->
        let init' = expand ~safe init in
        let e' = expand ~safe e in
        expand ~safe:false @@
        mk_array_fold_left q init' e'
    | ArrayMapBy(n,q,e) -> 
        let e' = expand ~safe e in
        expand ~safe:false @@
        mk_array_map n q e'
    )


let expand_circuit (c : TMACLE.circuit) : TMACLE.circuit = 
  {c with e = expand ~safe:true c.e}
