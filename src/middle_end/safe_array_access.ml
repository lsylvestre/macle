open Ast
open TMACLE
open Types

open Gensym

(* wrap array_get and array_set
   into a safe error handling to manage index out of bounds

*)

let is_litteral_integer e =
  match e with
  | Const (Int n),_ -> true
  | _ -> false


let expand e =

  let rec mapper ~default (env:unit) ((desc,ty) as e) =
    match desc with
    | CamlPrim(ArrayAccess{arr;idx}) when is_litteral_integer idx ->

        let arr = mapper ~default env arr in
        let idx = mapper ~default env idx in

        let x_arr = gensym "x" in
        let y_idx = gensym "y" in
        let z = gensym "z" in
        let err = Raise (Exception_Invalid_arg "Index out of bounds"),ty in
        mk_let [(x_arr,ty_of arr),arr;
                (y_idx,t_int),idx] @@
        mk_if (mk_binop ~ty:t_bool Lt (Var y_idx,t_int) (mk_int 0)) err @@
        mk_let1 (z,t_int) (mk_array_length (Var x_arr, ty_of arr)) @@
        mk_if (mk_binop ~ty:t_bool Ge (Var y_idx,t_int) (Var z,t_int)) err
        @@
        (CamlPrim(ArrayAccess{
                  arr = (Var x_arr,ty_of arr) ;
                  idx = (Var y_idx,t_int)
                }),ty)

    | CamlPrim(ArrayAssign{arr;idx;e}) when is_litteral_integer idx ->

        let arr = mapper ~default env arr in
        let idx = mapper ~default env idx in
        let e = mapper ~default env e in

        let x_arr = gensym "x" in
        let y_idx = gensym "y" in
        let z = gensym "z" in
        let err = Raise (Exception_Invalid_arg "Index out of bounds"),ty in
        mk_let [(x_arr,ty_of arr), arr;
                (y_idx,t_int), idx] @@
        mk_if (mk_binop ~ty:t_bool Lt (Var y_idx,t_int) (mk_int 0)) err @@
        mk_let1 (z,t_int) (mk_array_length (Var x_arr, ty_of arr)) @@
        mk_if (mk_binop ~ty:t_bool Ge (Var y_idx,t_int) (Var z,t_int)) err
        @@
        (CamlPrim(ArrayAssign{
                  arr = (Var x_arr,ty_of arr) ;
                  idx = (Var y_idx,t_int) ;
                  e
                }),ty)

    | _ -> default env e
  in
  Ast_mapper.map mapper () e


let safe_array_access_circuit (c : TMACLE.circuit) : TMACLE.circuit =
  {c with e = expand c.e}
