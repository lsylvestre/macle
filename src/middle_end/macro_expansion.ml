open Ast
open TMACLE
open Types

open Gensym

(* expand :
   - [or e1 e2] into [if e1 then true else e2]
   - [and e1 e2] into [if e1 then e2 else false]
   - array/list combinators
   - flat_array map/reduce/update
*)

let mk_array_fold_left f init earr =
  let tarr_ty = ty_of earr in
  let ty = array_ty tarr_ty in
  let tyr = ty_of init in
  let f' = gensym "aux" in
  let y = gensym "arr" in
  let n = gensym "size" in
  let i = gensym "idx" in
  let x = gensym "x" in
  let acc = gensym "acc" in
  let acc2 = gensym "acc2" in

  let var_y = Var y,tarr_ty in
  mk_let1 (y,tarr_ty) earr @@
  mk_let1 (n,t_int) (mk_array_length var_y) @@
  mk_letrec1 f' [(acc,tyr);(i,t_int)] (
    mk_if (mk_binop ~ty:t_bool Ge (Var i,t_int) (Var n,t_int))
      (Var acc,tyr) @@
      mk_let1 (x,ty) (mk_array_access var_y (Var i,t_int)) @@
      mk_let1 (acc2,tyr) (mk_app ~ty:tyr f [Var acc,tyr;Var x,ty]) @@
    mk_app ~ty:tyr f'
      [Var acc2,tyr; mk_binop ~ty:t_int Add (Var i,t_int) (mk_int 1)]
  ) @@
  mk_app ~ty:tyr f' [init; mk_int 0]


let mk_array_map n f e =
  let ty = ty_of e in
  let ty_elem = array_ty ty in
  let f' = gensym "aux" in
  let y = gensym "arr" in
  let size = gensym "size" in
  let i = gensym "idx" in
  let elem = gensym "element" in

  let var_y = (Var y,ty) in
  let var_i = Var i,t_int in
  let var_size = Var size,t_int in
  mk_let1 (y,ty) e @@
  mk_let1 (size,t_int) (mk_array_length var_y) @@
  mk_letrec1 f' [(i,t_int)]
    (mk_if (mk_binop ~ty:t_bool Ge var_i var_size) mk_unit @@
     mk_if (mk_binop ~ty:t_bool Gt var_i (mk_binop ~ty:t_int Sub var_size (mk_int n)))
       (mk_let1  (elem,ty_elem) (mk_array_access var_y var_i) @@
        mk_seq
          (mk_array_assign var_y var_i (mk_app ~ty:ty_elem f [Var elem,ty_elem]))
          (mk_app ~ty:t_unit f' [mk_binop ~ty:t_int Add var_i (mk_int 1)])) @@

     (let bs = List.init n (fun k ->
          ((elem^string_of_int k,ty_elem),
           (mk_array_access var_y
              (mk_binop ~ty:t_int Add var_i (mk_int k)))))
      in
      mk_let_cascad bs @@
      mk_let
        (List.init n (fun k -> (elem^string_of_int k,ty_elem),
                               mk_app ~ty f [Var(elem^string_of_int k),ty_elem])) @@
      mk_seqs (List.init n (fun k ->
          (mk_array_assign var_y (mk_binop ~ty:t_int Add var_i (mk_int k))
             (Var (elem^string_of_int k),ty_elem)))) @@
      (mk_app ~ty:t_unit f' [mk_binop ~ty:t_int Add var_i (mk_int n)])))
    (mk_app ~ty:t_unit f' [mk_int 0])


(* [mk_array_iter_by n f e] is expanded into :

   [let y = e in
    let size = Array.length y in
    let rec aux i =
      if size - i < n then raise fail else
      if i < size then f (copy x i n);aux (i+n) else
      ()]
*)
let mk_array_iter_by n f e =
  let ty = ty_of e in
  let tye = array_ty ty in
  let y = gensym "arr" in
  let i = gensym "idx" in
  let size = gensym "size" in
  let f' = gensym "aux" in
  let var_y = (Var y,ty) in
  let var_i = Var i,t_int in
  let var_size = Var size,t_int in
  mk_let1 (y,ty) e @@
  mk_let1 (size,t_int) (mk_array_length var_y) @@
  mk_letrec1 f' [i,t_int]
    (mk_if (mk_binop ~ty:t_bool Lt (mk_binop ~ty:t_bool Sub var_size var_i) (mk_int n))
       (Raise (Exception_Invalid_arg "Index out of bounds"),t_unit) @@
     (mk_seq (mk_app ~ty:t_unit f [FlatArrayOp (ArraySub(var_y,var_i,n)),flat_array_ tye (TSize n)])
        (mk_if (mk_binop ~ty:t_bool Lt var_i (mk_binop ~ty:t_int Sub var_size (mk_int n)))
           (mk_app ~ty:t_unit f' [mk_binop ~ty:t_int Add var_i (mk_int n)])
           mk_unit)))
    (mk_app ~ty:t_unit f' [mk_int 0])


(* [mk_array_iter_by n f e] is expanded into :

   [let y = e in
    let size = Array.length y in
    let rec aux i acc =
      if size - i < n then raise fail else
      if i < size then aux (i+n) (f acc (copy x i n)) else
      acc]
*)

let mk_array_reduce_by n f init e =
  let tye = ty_of e in
  let ty_elem = array_ty tye in
  let ty_acc = ty_of init in
  let y = gensym "arr" in
  let acc = gensym "acc" in
  let i = gensym "idx" in
  let size = gensym "size" in
  let f' = gensym "aux" in
  let var_y = (Var y,tye) in
  let var_i = Var i,t_int in
  let var_acc = Var acc,ty_acc in
  let var_size = Var size,t_int in
  (* ajouter un test [size % n] au début avec lancé d'exception si la taille du tableau n'est pas un multiple de la taille d'un paquet *)
  mk_let1 (y,tye) e @@
  mk_let1 (size,t_int) (mk_array_length var_y) @@
  mk_letrec1 f' [(i,t_int);(acc,ty_acc)]
    (* (mk_if (Macro(LazyAnd(mk_binop ~ty:t_bool Lt (mk_binop ~ty:t_bool Sub var_size var_i) (mk_int n),
                          mk_binop ~ty:t_bool Neq var_size var_i)),t_bool)
        (Raise (Exception_Invalid_arg "Index out of bounds"),t_unit) @@ *)
    (mk_if (mk_binop ~ty:t_bool Le var_i (mk_binop ~ty:t_int Sub var_size (mk_int n)))
       (mk_app ~ty:t_unit f' [mk_binop ~ty:t_int Add var_i (mk_int n);
                              (mk_app ~ty:t_unit f [var_acc; FlatArrayOp (ArraySub(var_y,var_i,n)),flat_array_ ty_elem (TSize n)])
                             ])
       var_acc) (* ) *)
    (mk_app ~ty:ty_acc f' [mk_int 0;init])



let mk_flat_array_update arr idx e =
  let n = flat_array_size (ty_of arr) in
  let ty_elem = ty_of e in
  let ty_arr = ty_of arr in
  let x = gensym "x" in
  mk_let1 (x,ty_arr) e @@
  let es =
    (match idx with
     | Const (Int n),_ ->
       List.init n (fun i ->
           if i = n then e else
             FlatArrayOp (FlatGet{e=(Var x,ty_arr);idx=mk_int i}),ty_elem)
     | _ ->
       List.init n (fun i ->
           mk_if (mk_binop ~ty:t_int Eq (mk_int i) idx) e
             (FlatArrayOp (FlatGet{e=(Var x,ty_arr);idx=mk_int i}),ty_elem))

    ) in
  FlatArrayOp(FlatMake es),ty_arr



let rec expand ~(safe:bool) (e:exp) : exp =

  let mapper ~default (env:unit) ((desc,ty) as e) =
    default env @@
    match desc with
    | Macro c ->
       (match c with
        | LazyOr(e1,e2) ->
            mk_if e1 (mk_bool true) e2
        | LazyAnd(e1,e2) ->
            mk_if e1 e2 (mk_bool false)
        | Map(f,es) ->
            let xs = List.map (fun (_,t) -> gensym "x",t) es in
            let ty_elem = flat_array_ty ty in
            let e0 = mk_app ~ty:ty_elem f (List.map (fun (x,t) -> Var x,t) xs)
            in
            FlatArrayOp(FlatMap((xs,e0),es)),ty
        | Reduce(f,init,e) ->
            let tye = ty_of e in
            let acc = gensym "acc" in
            let x = gensym "x" in
            let e0 = mk_app ~ty f [Var acc,ty;Var x,tye] in
            FlatArrayOp(FlatReduce(((acc,ty),(x,tye),e0),init,e)),ty
        | ArrayUpdate{arr;idx;e} ->
            mk_flat_array_update arr idx e
        | OCamlArrayReduceBy(n,f,init,e) ->
            mk_array_reduce_by n f init e
        | OCamlArrayIterBy(n,f,e) ->
            mk_array_iter_by n f e
        | OCamlArrayMapBy(n,f,e) ->
            mk_array_map n f e
        | OCamlArrayFoldLeft(f,init,e) ->
            mk_array_fold_left f init e)
    | _ ->
      e
  in
  Ast_mapper.map mapper () e


let expand_circuit (c : TMACLE.circuit) : TMACLE.circuit =
  {c with e = expand ~safe:true c.e}
