open Ast
open TMACLE
open Types

open Gensym

let mk_array_reduce_without_packet f init earr =
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


let mk_array_map n f e1 e2 =
  let ty = ty_of e1 in
  let tye2 = ty_of e2 in
  let tye1 = array_ty ty in
  let y = gensym "arr" in
  let y2 = gensym "arr" in
  let i = gensym "idx" in
  let size = gensym "size" in
  let size2 = gensym "size" in
  let f' = gensym "aux" in
  let var_y = (Var y,ty) in
  let var_y2 = (Var y2,tye2) in
  let var_i = Var i,t_int in
  let var_size = Var size,t_int in
  let var_size2 = Var size2,t_int in
  mk_let1 (y,ty) e1 @@
  mk_let1 (y2,tye2) e2 @@
  mk_let1 (size,t_int) (mk_array_length var_y) @@
  mk_let1 (size2,t_int) (mk_array_length var_y2) @@
  mk_if (mk_binop ~ty:t_bool Neq var_size var_size2)
    (Raise (Exception_Invalid_arg "map: source and destination arrays must have a same size"),t_unit) @@
  mk_letrec1 f' [i,t_int]
    (mk_if (mk_binop ~ty:t_bool Lt (mk_binop ~ty:t_bool Sub var_size var_i) (mk_int n))
       (Raise (Exception_Invalid_arg "Index out of bounds"),t_unit)
       (mk_seq (PacketPrim(OfPacket(
            mk_pk_map ~ty:tye1 f 
              (PacketPrim (ToPacket(var_y,var_i,n)), packet_ tye1 (TSize n)),
            var_y2,
            var_i,
            n)),t_unit)
           (mk_if (mk_binop ~ty:t_bool Lt var_i (mk_binop ~ty:t_int Sub var_size (mk_int n)))
              (mk_app ~ty:t_unit f' [mk_binop ~ty:t_int Add var_i (mk_int n)])
              mk_unit)))
    (mk_app ~ty:t_unit f' [mk_int 0])


let mk_array_reduce n f init e =
  let tye = ty_of e in
  let ty_elem = array_ty tye in
  let ty_acc = ty_of init in
  let y    = gensym "arr" in
  let acc  = gensym "acc" in
  let i    = gensym "idx" in
  let size = gensym "size" in
  let f'   = gensym "aux" in
  let var_y = (Var y,tye) in
  let var_i = Var i,t_int in
  let var_acc = Var acc,ty_acc in
  let var_size = Var size,t_int in
  mk_let1 (y,tye) e @@
  mk_let1 (size,t_int) (mk_array_length var_y) @@
  mk_letrec1 f' [(i,t_int);(acc,ty_acc)]
    (mk_if (mk_binop ~ty:t_bool Le var_i (mk_binop ~ty:t_int Sub var_size (mk_int n)))
       (mk_app ~ty:t_unit f' [mk_binop ~ty:t_int Add var_i (mk_int n);
                              (mk_pk_reduce f var_acc (PacketPrim (ToPacket(var_y,var_i,n)),packet_ ty_elem (TSize n)))])
       var_acc) (* ) *)
    (mk_app ~ty:ty_acc f' [mk_int 0;init])



let mk_array_scan n f init e e2 =
  let tye = ty_of e in
  let tye2 = ty_of e2 in
  let ty_elem = array_ty tye in
  let ty_acc = ty_of init in
  let y    = gensym "arr" in
  let y2   = gensym "arr" in
  let acc  = gensym "acc" in
  let i    = gensym "idx" in
  let size = gensym "size" in
  let size2 = gensym "size" in
  let f'   = gensym "aux" in
  let var_y = (Var y,tye) in
  let var_y2 = (Var y2,tye2) in
  let var_i = Var i,t_int in
  let var_acc = Var acc,ty_acc in
  let var_size = Var size,t_int in
  let var_size2 = Var size2,t_int in
  mk_let1 (y,tye) e @@
  mk_let1 (y2,tye2) e2 @@
  mk_let1 (size,t_int) (mk_array_length var_y) @@
  mk_let1 (size2,t_int) (mk_array_length var_y2) @@
  mk_if (mk_binop ~ty:t_bool Neq var_size var_size2)
    (Raise (Exception_Invalid_arg "scan: source and destination arrays must have a same size"),t_unit) @@
  mk_letrec1 f' [(i,t_int);(acc,ty_acc)]
    (* todo: (mk_if (Macro(LazyAnd(mk_binop ~ty:t_bool Lt (mk_binop ~ty:t_bool Sub var_size var_i) (mk_int n),
                           mk_binop ~ty:t_bool Neq var_size var_i)),t_bool)
        (Raise (Exception_Invalid_arg "Index out of bounds"),t_unit) @@ *)
    (mk_seq (PacketPrim(OfPacket(
         (mk_pk_scan f var_acc (PacketPrim (ToPacket(var_y,var_i,n)),packet_ ty_elem (TSize n))),            
         var_y2,
         var_i,
         n)),t_unit) @@
     (mk_if (mk_binop ~ty:t_bool Lt var_i (mk_binop ~ty:t_int Sub var_size (mk_int n)))
        (mk_app ~ty:t_unit f' [mk_binop ~ty:t_int Add var_i (mk_int n);
                               (mk_pk_reduce f var_acc (PacketPrim (ToPacket(var_y,var_i,n)),packet_ ty_elem (TSize n)))])
        mk_unit)) (* ) *)
    (mk_app ~ty:t_unit f' [mk_int 0;init])




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
        | OCamlArrayReduce(n,f,init,e) ->
            mk_array_reduce n f init e
        | OCamlArrayMap(n,f,e1,e2) ->
            mk_array_map n f e1 e2
        | OCamlArrayScan(n,f,init,e,dst) ->
            mk_array_scan n f init e dst)
    | _ ->
      e
  in
  Ast_mapper.map mapper () e


let expand_circuit (c : TMACLE.circuit) : TMACLE.circuit =
  {c with e = expand ~safe:true c.e}
