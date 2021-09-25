
open Format

type ty = 
| TConst of tconst
| TCamlRef of ty
| TCamlArray of ty
| TCamlList of ty
| TPtr               (* pointeur *)
| TVar of tvar ref
| TFun of ty list * ty
and tconst = TStd_logic | TBool | TInt | TUnit
and tvar = V of int | Ty of ty

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
  | TCamlRef ty ->
      fprintf fmt "(%a) ref"
         print_ty ty
  | TCamlArray ty ->
      fprintf fmt "(%a) array"
         print_ty ty
  | TCamlList ty ->
      fprintf fmt "(%a) list"
         print_ty ty
  | TPtr ->
      pp_print_text fmt "ptr"
  | TVar{contents=V n} -> 
      fprintf fmt "'a%d" n
  | TVar{contents=Ty t} -> 
      fprintf fmt "{tvar <- %a}" print_ty t
  | TFun(ts,t) ->
      fprintf fmt "(";
      List.iter (print_ty fmt) ts;
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
  | TCamlRef t -> TCamlRef (canon t)
  | TCamlArray t -> TCamlArray (canon t)
  | TCamlList t -> TCamlList (canon t)
  | TFun(ts,t) -> TFun (List.map canon ts, canon t)
  | (TConst _ | TPtr) -> t


let rec occur n t = 
  match t with
  | TVar {contents=V m} -> n = m
  | TVar {contents=Ty _} -> false
  | (TCamlRef t | TCamlArray t | TCamlList t) -> occur n t
  | TFun(ts,t) -> List.for_all (occur n) ts && occur n t
  | (TConst _ | TPtr) -> false
