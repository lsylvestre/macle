open Ktypes
open Format

let rec print_ty fmt ty = 
  let open Format in 
  match ty with
  | TConst c -> 
    (match c with
    | TStd_logic -> 
      pp_print_text fmt "std_logic"
    | TBool -> 
        pp_print_text fmt "bool"
    | TInt -> 
        pp_print_text fmt "int"
    | TUnit -> 
        pp_print_text fmt "unit")
  | TPtr (c,[]) ->
     pp_print_text fmt c
  | TPtr (c,tys) ->
     pp_print_text fmt "(";
     pp_print_list 
          ~pp_sep:(fun fmt () -> fprintf fmt ",") print_ty fmt tys;
     fprintf fmt ") %s" c;
  | TVar n -> 
      fprintf fmt "'a%d" n
