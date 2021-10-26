open Format
open Kast
open Ast

module PP_atom = Pprint_kast.PP_atom

open TMACLE
open Types

let pp_ident fmt x =
  pp_print_text fmt String.(concat "_0x" @@ split_on_char '#' x)

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
      List.iter (fun ty -> print_ty fmt ty; fprintf fmt "->") ts;  (* manque "*" *)
      print_ty fmt t;
      fprintf fmt ")"

let rec pp_exp fmt (e,ty) = 
  match e with
  | Var x -> 
      pp_ident fmt x
  | Const c -> 
      PP_atom.pp_const fmt c
  | Prim (Binop p,[e1;e2]) ->
    fprintf fmt "(%a %a %a)"
      pp_exp e1
      PP_atom.pp_binop p
      pp_exp e2
| Prim (Unop Mod2,[e]) ->
    fprintf fmt "(%a mod 2)"
      pp_exp e
| Prim (Unop DivBy2,[e]) ->
    fprintf fmt "(%a / 2)"
      pp_exp e
| Prim (Unop p,[e]) ->
        fprintf fmt "(%a %a)"
          PP_atom.pp_unop p
          pp_exp e
| Prim _ -> 
    assert false
| If (e1,e2,e3) ->
    fprintf fmt "(if %a@ then @[<hov>%a@]@ else @[<hov>%a@])@,"
      pp_exp e1
      pp_exp e2
      pp_exp e3
| Case (e1,hs,e2) ->
    let pp_handle fmt (c,e) = 
      fprintf fmt "%a -> %a" PP_atom.pp_const c pp_exp e 
    in
    fprintf fmt "(case (%a : %a) with" pp_exp e1 print_ty (ty_of e1);
    pp_print_list 
      ~pp_sep:(fun fmt () -> fprintf fmt "@,| ") pp_handle fmt hs;
    fprintf fmt "_ -> %a)" pp_exp e2;
| App (q,es) ->
    fprintf fmt "(%a " pp_ident q;
    pp_print_list 
      ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_exp fmt es;
     fprintf fmt ")@,"
| Let(bs,e) -> 
    fprintf fmt "@[<v>(let ";
    pp_print_list 
        ~pp_sep:(fun fmt () -> fprintf fmt "@, and ") 
         (fun fmt ((x,ty),e) -> 
             fprintf fmt "%a : %a = %a" pp_ident x print_ty ty pp_exp e) fmt bs;
     fprintf fmt "@,@]@,in %a)" pp_exp e
| LetFun(t,e) ->
    fprintf fmt "(@[<v 2>let ";
    pp_transition fmt t;
    fprintf fmt "@ in ";
    pp_exp fmt e;
    fprintf fmt ")@]"
| LetRec(ts,e) ->
    fprintf fmt "(@[<v>@[<v 2>let rec ";
     pp_print_list 
        ~pp_sep:(fun fmt () -> fprintf fmt "@]@,@[<v 2>and ") 
       pp_transition fmt ts;
    fprintf fmt "@]in (";
    pp_exp fmt e;
    fprintf fmt " : %a" print_ty (ty_of e);
    fprintf fmt "))@]"
| CamlPrim e -> 
(match e with
| RefAccess e -> 
    fprintf fmt "!(%a : %a)"
      pp_exp e print_ty (ty_of e)
| RefAssign {r;e} -> 
    fprintf fmt "(%a : %a) := (%a)"
      pp_exp r
      print_ty (ty_of r)
      pp_exp e 
| ArrayAccess{arr;idx} -> 
    fprintf fmt "(%a : %a).(%a)"
      pp_exp arr
      print_ty (ty_of arr)
      pp_exp idx
| ArrayAssign{arr;idx;e} -> 
    fprintf fmt "(%a : %a).(%a) <- (%a)"
      pp_exp arr
      print_ty (ty_of arr)
      pp_exp idx
      pp_exp e
| ArrayLength e ->
    fprintf fmt "(Array.length (%a : %a))"
      pp_exp e
      print_ty (ty_of e)
| ListHd e ->
    fprintf fmt "(List.hd (%a : %a))"
      pp_exp e
      print_ty (ty_of e)
| ListTl e ->
    fprintf fmt "(List.tl (%a : %a))"
      pp_exp e
      print_ty (ty_of e)
| ArrayMapBy(n,x,e) ->
   let y = Gensym.gensym "y" in
   let z = Gensym.gensym "z" in
   let i = Gensym.gensym "i" in
   fprintf fmt "(let %a : %a = %a in@," pp_ident y print_ty (ty_of e) pp_exp e;
   fprintf fmt "(Array.iteri (fun %a %a -> %a.(%a) <- %a (%a.(%a))) %a))" 
     pp_ident i pp_ident z pp_ident y 
     pp_ident i pp_ident x pp_ident y 
     pp_ident i pp_ident y
| ArrayFoldLeft(x,acc,e) ->
    fprintf fmt "(Array.fold_left %a (%a : %a) (%a : %a))" 
      pp_ident x 
      pp_exp acc 
      print_ty (ty_of acc)
      pp_exp e
      print_ty (ty_of e)
| ListFoldLeft(x,acc,e) ->
    fprintf fmt "(List.fold_left %a (%a : %a) (%a : %a))" 
      pp_ident x 
      pp_exp acc 
      print_ty (ty_of acc) 
      pp_exp e
      print_ty (ty_of e)
)
and pp_transition fmt ((q,xs),e) = 
  fprintf fmt "@[<b>%a@ " pp_ident q; 
  pp_print_list 
    ~pp_sep:(fun fmt () -> fprintf fmt "@ ") 
       pp_tyconstr fmt xs;
  fprintf fmt " =@]@,";
  pp_exp fmt e;

and pp_tyconstr fmt (x,ty) = 
  fprintf fmt "(%a : %a)" pp_ident x print_ty ty

let pp_circuit fmt {x;xs;e} =
  fprintf fmt "@[<v 2>@[<b>let %a@ " pp_ident x;
  pp_print_list 
    ~pp_sep:(fun fmt () -> fprintf fmt "@ ") pp_tyconstr fmt xs;
  fprintf fmt "@ @]=@,";
  pp_exp fmt e;
  fprintf fmt "@ ;;@,@]@,"
