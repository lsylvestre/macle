
open Format

type name = string

type ty = 
| TConst of tconst
| TConstr of name * ty list
| TPtr               (* pointeur *)
| TVar of tvar ref
| TFun of ty list * ty
and tconst = TStd_logic | TBool | TInt | TUnit
and tvar = V of int | Ty of ty



let list_,ref_,array_ = 
  let list_name = "list" 
  and ref_name = "ref"
  and array_name = "array" in
  (fun v -> TConstr(list_name,[v])),
  (fun v -> TConstr(ref_name,[v])),
  (fun v -> TConstr(array_name,[v]))

module Tenv = Hashtbl;;

let rec print_ty fmt ty = 
  let open Format in 
  match ty with
  | TConst tc -> 
    (match tc with
    | TStd_logic -> 
        pp_print_text fmt "std_logic"
    | TBool -> 
        pp_print_text fmt "bool"
    | TInt -> 
        pp_print_text fmt "int"
    | TUnit -> 
        pp_print_text fmt "unit")
  | TConstr (x,[]) -> 
      pp_print_text fmt x
  | TConstr (x,tys) ->
      fprintf fmt "(";
      pp_print_list 
      ~pp_sep:(fun fmt () -> fprintf fmt ", ") 
         print_ty fmt tys;
      fprintf fmt ") %s" x;
  | TPtr ->
      pp_print_text fmt "ptr"
  | TVar{contents=V n} -> 
      fprintf fmt "'a%d" n
  | TVar{contents=Ty t} -> 
      fprintf fmt "{tvar <- %a}" print_ty t
  | TFun(ts,t) ->
      fprintf fmt "(";
      List.iter (print_ty fmt) ts;  (* manque "*" *)
      fprintf fmt "->";
      print_ty fmt t;
      fprintf fmt ")"

let is_type_variable {contents=t} =
  match t with
  | V _ -> true
  | _ -> false

let as_type_variable {contents=t} =
  match t with
  | V n -> n
  | _ -> invalid_arg "Types.as_type_variable"

let print_env fmt env = 
  Format.fprintf fmt "\n\n[";
  Tenv.iter (fun x t -> Format.fprintf fmt "(%s, %a);" x print_ty t) env;
  Format.fprintf fmt "]\n"

let newvar = 
  let c = ref 0 in
  fun () -> 
     let ty = TVar (ref (V (!c))) in
     incr c; ty

let rec canon t =
  match t with 
  | TVar({contents=Ty t'} as v) -> 
      let t2 = canon t' in 
      v := Ty t2; t2
  | TVar{contents=V _} -> t
  | TConstr (x,tys) -> TConstr (x,List.map canon tys)
  | TFun(ts,t) -> TFun (List.map canon ts, canon t)
  | (TConst _ | TPtr) -> t


let rec occur n t = 
  match t with
  | TVar {contents=V m} -> n = m
  | TVar {contents=Ty _} -> false
  | TConstr (_,tys) -> List.exists (occur n) tys
  | TFun(ts,t) -> List.for_all (occur n) ts && occur n t
  | (TConst _ | TPtr) -> false
