open Format
open Kast
open Ast

module PP_atom = Pprint_kast.PP_atom
  
let pp_state = pp_print_text

module PP_MACLE = struct
  open PP_atom
  open MACLE

  let rec pp_exp fmt (e,_) = 
    match e with
    | Var x -> 
        pp_print_text fmt x
    | Const c -> 
        pp_const fmt c
    | Prim (Binop p,[e1;e2]) ->
      fprintf fmt "(%a %a @[<v>%a@])"
        pp_exp e1
        PP_atom.pp_binop p
        pp_exp e2
  | Prim (Unop p,[e]) ->
          fprintf fmt "(%a %a)"
            PP_atom.pp_unop p
            pp_exp e
  | Prim _ -> 
      assert false
  | If (e1,e2,e3) ->
      fprintf fmt "if %a@ then @[<hov>%a@]@ else @[<hov>%a@]@,"
        pp_exp e1
        pp_exp e2
        pp_exp e3
  | Case (e1,hs,e2) ->
      let pp_handle fmt (c,e) = 
        fprintf fmt "%a -> %a" pp_const c pp_exp e 
      in
      fprintf fmt "case %a with" pp_exp e1;
      pp_print_list 
        ~pp_sep:(fun fmt () -> fprintf fmt "@,| ") pp_handle fmt hs;
      fprintf fmt "otherwise %a" pp_exp e2;
  | App (q,es) ->
      fprintf fmt "%s(" q;
      pp_print_list 
        ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp_exp fmt es;
       fprintf fmt ")@,"
  | Let(bs,e) -> 
    fprintf fmt "@[<v>let ";
    pp_print_list 
        ~pp_sep:(fun fmt () -> fprintf fmt "@, and ") 
         (fun fmt (x,e) -> 
             fprintf fmt "@[<v 2>%s =@,%a@]" x pp_exp e) fmt bs;
     fprintf fmt "@ in@,%a@]" pp_exp e
  | LetFun(t,e) ->
    fprintf fmt "(@[<v>let ";
    pp_transition fmt t;
    fprintf fmt "in ";
    pp_exp fmt e;
    fprintf fmt ")@]"
  | LetRec(ts,e) ->
    fprintf fmt "(@[<v 2>let rec ";
     pp_print_list 
        ~pp_sep:(fun fmt () -> fprintf fmt "@,and ") 
       pp_transition fmt ts;
    fprintf fmt " in ";
    pp_exp fmt e;
    fprintf fmt ")@]"
  | CamlPrim e -> 
  (match e with
  | RefAccess e -> 
      fprintf fmt "!(%a)"
        pp_exp e
  | RefAssign {r;e} -> 
      fprintf fmt "(%a) := (%a)"
        pp_exp r
        pp_exp e
  | ArrayAccess{arr;idx} -> 
      fprintf fmt "(%a).(%a)"
        pp_exp arr
        pp_exp idx
  | ArrayAssign{arr;idx;e} -> 
      fprintf fmt "(%a).(%a) <- (%a)"
        pp_exp arr
        pp_exp idx
        pp_exp e
  | ArrayLength e ->
      fprintf fmt "array_length (%a)"
        pp_exp e
  | ListHd e ->
      fprintf fmt "list_hd (%a)"
        pp_exp e
  | ListTl e ->
      fprintf fmt "list_tl (%a)"
        pp_exp e
  | ArrayMapBy(n,x,e) ->
     fprintf fmt "array_map_by %d %s %a" n x 
        pp_exp e
  | ArrayFoldLeft(x,acc,e) ->
      fprintf fmt "array_fold_left %s %a %a" x 
        pp_exp acc 
        pp_exp e
  | ListFoldLeft(x,acc,e) ->
      fprintf fmt "list_fold_left %s %a %a" x 
        pp_exp acc 
        pp_exp e
  )

  and pp_transition fmt ((q,xs),e) = 
    fprintf fmt "%s(" q; 
    pp_print_list 
      ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_print_text fmt xs;
    fprintf fmt ") = @,";
    pp_exp fmt e

  let pp_circuit fmt {x;xs;e} =
    fprintf fmt "@[<v>@[<v 2>circuit %s(" x;
    pp_print_list 
      ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_print_text fmt xs;
    fprintf fmt ") =@,";
    pp_exp fmt e;
    fprintf fmt "@] ;;@,@,@]"
end


module PP_TMACLE = struct
  open PP_atom
  open TMACLE
  open Types

  let rec pp_exp fmt e = 
    match e with
    | Var x -> 
        pp_print_text fmt x
    | Const c -> 
        pp_const fmt c
    | Prim (Binop p,[e1;e2]) ->
      fprintf fmt "(%a %a @[<v>%a@])"
        pp_exp e1
        PP_atom.pp_binop p
        pp_exp e2
  | Prim (Unop p,[e]) ->
          fprintf fmt "(%a %a)"
            PP_atom.pp_unop p
            pp_exp e
  | Prim _ -> 
      assert false
  | If (e1,e2,e3,_) ->
      fprintf fmt "if %a@ then @[<hov>%a@]@ else @[<hov>%a@]@,"
        pp_exp e1
        pp_exp e2
        pp_exp e3
  | Case (e1,ty,hs,e2,_) ->
      let pp_handle fmt (c,e) = 
        fprintf fmt "%a -> %a" pp_const c pp_exp e 
      in
      fprintf fmt "case (%a : %a) with" pp_exp e1 print_ty ty;
      pp_print_list 
        ~pp_sep:(fun fmt () -> fprintf fmt "@,| ") pp_handle fmt hs;
      fprintf fmt "otherwise %a" pp_exp e2;
  | App (q,es,_) ->
      fprintf fmt "%s(" q;
      pp_print_list 
        ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_exp fmt es;
       fprintf fmt ")@,"
  | Let(bs,e,_) -> 
      fprintf fmt "@[<v>let ";
      pp_print_list 
          ~pp_sep:(fun fmt () -> fprintf fmt "@, and ") 
           (fun fmt ((x,ty),e) -> 
               fprintf fmt "@[<v 2>%s : %a =@,%a@]" x print_ty ty pp_exp e) fmt bs;
       fprintf fmt "@ in@,%a" pp_exp e
  | LetFun(t,e) ->
      fprintf fmt "@[<v>(let ";
      pp_transition fmt t;
      fprintf fmt "in ";
      pp_exp fmt e;
      fprintf fmt ")@]"
  | LetRec(ts,e,ty) ->
      fprintf fmt "@[<v>(@[<v 2>let rec ";
       pp_print_list 
          ~pp_sep:(fun fmt () -> fprintf fmt "@,and ") 
         pp_transition fmt ts;
      fprintf fmt " in (";
      pp_exp fmt e;
      fprintf fmt " : %a" print_ty ty;
      fprintf fmt "))@]"
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
      fprintf fmt "array_length (%a : %a array)"
        pp_exp e
        print_ty ty
  | ListHd (e,ty) ->
      fprintf fmt "list_hd (%a : %a list)"
        pp_exp e
        print_ty ty
  | ListTl (e,ty) ->
      fprintf fmt "list_tl (%a : %a list)"
        pp_exp e
        print_ty ty
  | ArrayMapBy(n,x,ty,e) ->
     fprintf fmt "array_map_by %d %s (%a : %a array)" n x 
        pp_exp e
        print_ty ty
  | ArrayFoldLeft(x,ty1,ty2,acc,e) ->
      fprintf fmt "array_fold_left %s (%a : %a) (%a : %a array)" x 
        pp_exp acc 
        print_ty ty1 
        pp_exp e
        print_ty ty2
  | ListFoldLeft(x,ty1,ty2,acc,e) ->
      fprintf fmt "list_fold_left %s (%a : %a) (%a : %a list)" x 
        pp_exp acc 
        print_ty ty1 
        pp_exp e
        print_ty ty2
  )
  and pp_transition fmt ((q,xs),e) = 
    fprintf fmt "%s(" q; 
    pp_print_list 
      ~pp_sep:(fun fmt () -> fprintf fmt ", ") 
         pp_tyconstr fmt xs;
    fprintf fmt ") = @,";
    pp_exp fmt e

  and pp_tyconstr fmt (x,ty) = 
    fprintf fmt "%s : %a" x print_ty ty

  let pp_circuit fmt {x;xs;e} =
    fprintf fmt "@[<v>@[<v 2>circuit %s(" x;
    pp_print_list 
      ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_tyconstr fmt xs;
    fprintf fmt ") =@,";
    pp_exp fmt e;
    fprintf fmt "@] ;;@,@,@]"
end
