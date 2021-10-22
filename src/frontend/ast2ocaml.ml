open Format
open Kast
open Ast

module PP_atom = Pprint_kast.PP_atom
  
let pp_state = pp_print_text

module PP_TMACLE = struct
  open PP_atom
  open TMACLE
  open Types

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

  let rec pp_exp fmt e = 
    match e with
    | Var x -> 
        pp_print_text fmt x
    | Const c -> 
        pp_const fmt c
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
  | If (e1,e2,e3,_) ->
      fprintf fmt "(if %a@ then @[<hov>%a@]@ else @[<hov>%a@])@,"
        pp_exp e1
        pp_exp e2
        pp_exp e3
  | Case (e1,ty,hs,e2,_) ->
      let pp_handle fmt (c,e) = 
        fprintf fmt "%a -> %a" pp_const c pp_exp e 
      in
      fprintf fmt "(case (%a : %a) with" pp_exp e1 print_ty ty;
      pp_print_list 
        ~pp_sep:(fun fmt () -> fprintf fmt "@,| ") pp_handle fmt hs;
      fprintf fmt "_ -> %a)" pp_exp e2;
  | App (q,es,_) ->
      fprintf fmt "(%s " q;
      pp_print_list 
        ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_exp fmt es;
       fprintf fmt ")@,"
  | Let(bs,e,_) -> 
      fprintf fmt "@[<v>let ";
      pp_print_list 
          ~pp_sep:(fun fmt () -> fprintf fmt "@, and ") 
           (fun fmt ((x,ty),e) -> 
               fprintf fmt "%s : %a = %a" x print_ty ty pp_exp e) fmt bs;
       fprintf fmt "@,@]@,in %a" pp_exp e
  | LetFun(t,e) ->
      fprintf fmt "@[<v>(let ";
      pp_transition fmt t;
      fprintf fmt "@ in ";
      pp_exp fmt e;
      fprintf fmt ")@]"
  | LetRec(ts,e) ->
      fprintf fmt "@[<v>(@[<v 2>let rec ";
       pp_print_list 
          ~pp_sep:(fun fmt () -> fprintf fmt "@,@[<v 2>and ") 
         pp_transition fmt ts;
      fprintf fmt "@]in ";
      pp_exp fmt e;
      fprintf fmt ")@]"
  | CamlPrim e -> 
  (match e with
  | RefAccess (e,ty) -> 
      fprintf fmt "!(%a : %a ref)"
        pp_exp e print_ty ty
  | RefAssign {r;e;ty} -> 
      fprintf fmt "(%a : %a ref) := (%a)"
        pp_exp r
        print_ty ty
        pp_exp e 
  | ArrayAccess{arr;idx;ty} -> 
      fprintf fmt "(%a : %a array).(%a)"
        pp_exp arr
        print_ty ty
        pp_exp idx
  | ArrayAssign{arr;idx;e;ty} -> 
      fprintf fmt "(%a : %a array).(%a) <- (%a)"
        pp_exp arr
        print_ty ty
        pp_exp idx
        pp_exp e
  | ArrayLength (e,ty) ->
      fprintf fmt "Array.length (%a : %a array)"
        pp_exp e
        print_ty ty
  | ListHd (e,ty) ->
      fprintf fmt "List.hd (%a : %a list)"
        pp_exp e
        print_ty ty
  | ListTl (e,ty) ->
      fprintf fmt "List.tl (%a : %a list)"
        pp_exp e
        print_ty ty
  | ArrayMapBy(n,x,ty,e) ->
     let y = Gensym.gensym "y" in
     let z = Gensym.gensym "z" in
     let i = Gensym.gensym "i" in
     fprintf fmt "(let %s : %a array = %a in@," y print_ty ty pp_exp e;
     fprintf fmt "Array.iteri (fun %s %s -> %s.(%s) <- %s (%s.(%s))) %s)" i z y i x y i y
  | ArrayFoldLeft(x,ty1,ty2,acc,e) ->
      fprintf fmt "Array.fold_left %s (%a : %a) (%a : %a array)" x 
        pp_exp acc 
        print_ty ty1 
        pp_exp e
        print_ty ty2
  | ListFoldLeft(x,ty1,ty2,acc,e) ->
      fprintf fmt "List.fold_left %s (%a : %a) (%a : %a list)" x 
        pp_exp acc 
        print_ty ty1 
        pp_exp e
        print_ty ty2
  )
  and pp_transition fmt ((q,xs),e) = 
    fprintf fmt "%s " q; 
    pp_print_list 
      ~pp_sep:(fun fmt () -> fprintf fmt " ") 
         pp_tyconstr fmt xs;
    fprintf fmt " = @,";
    pp_exp fmt e;
    fprintf fmt "@]"

  and pp_tyconstr fmt (x,ty) = 
    fprintf fmt "(%s : %a)" x print_ty ty

  let pp_circuit fmt {x;xs;e} =
    fprintf fmt "@[<v 2>let %s " x;
    pp_print_list 
      ~pp_sep:(fun fmt () -> fprintf fmt "@ ") pp_tyconstr fmt xs;
    fprintf fmt " =@,";
    pp_exp fmt e;
    fprintf fmt "@]"
end

let pp_circuit = PP_TMACLE.pp_circuit