open Format
open Kast
open Ast

let pp_state = pp_print_text

module PP_MACLE = struct
  open Pprint_atom
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
        pp_binop p
        pp_exp e2
  | Prim (Unop p,[e]) ->
          fprintf fmt "(%a %a)"
            pp_unop p
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
         (fun fmt ((x,_),e) -> 
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
  | Match(e,cases) -> 
    let pp_case fmt (c,xs,e) = 
      let pp_pat fmt = function
      | None,_ -> fprintf fmt "_"
      | Some x,_ -> fprintf fmt "%a" pp_ident x in
      let pp_pats fmt = function
      | [] -> ()
      | xs ->  
        pp_print_text fmt "(";
        pp_print_list 
         ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_pat fmt xs;
        pp_print_text fmt ")" in
      fprintf fmt "%s%a -> %a" c pp_pats xs pp_exp e;    
    in
    fprintf fmt "(@[<v>match %a with" pp_exp e;
    pp_print_list 
      ~pp_sep:(fun fmt () -> fprintf fmt "@,| ") pp_case fmt cases;
    fprintf fmt ")@]"
  | Raise exc -> 
    (match exc with
    | Exception_Failure s -> 
        fprintf fmt "(raise (Failure \"%s\"))" s
    | Exception_Invalid_arg s -> 
        fprintf fmt "(raise (Invalid_arg \"%s\"))" s)
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
      ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_print_text fmt (List.map fst xs);
    fprintf fmt ") = @,";
    pp_exp fmt e

  let pp_circuit fmt {x;xs;e} =
    fprintf fmt "@[<v>@[<v 2>circuit %s(" x;
    pp_print_list 
      ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_print_text fmt (List.map fst xs);
    fprintf fmt ") =@,";
    pp_exp fmt e;
    fprintf fmt "@] ;;@,@,@]"
end


module PP_TMACLE = struct
  open Pprint_atom
  open TMACLE
  open Types

  let rec pp_exp fmt (e,ty) = 
    match e with
    | Var x -> 
        pp_print_text fmt x
    | Const c -> 
        pp_const fmt c
    | Prim (Binop p,[e1;e2]) ->
      fprintf fmt "(%a %a @[<v>%a@])"
        pp_exp e1
        pp_binop p
        pp_exp e2
  | Prim (Unop p,[e]) ->
          fprintf fmt "(%a %a)"
            pp_unop p
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
      fprintf fmt "case (%a : %a) with" pp_exp e1 print_ty (ty_of e1);
      pp_print_list 
        ~pp_sep:(fun fmt () -> fprintf fmt "@,| ") pp_handle fmt hs;
      fprintf fmt "otherwise %a" pp_exp e2;
  | App (q,es) ->
      fprintf fmt "%s(" q;
      pp_print_list 
        ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_exp fmt es;
       fprintf fmt ")@,"
  | Let(bs,e) -> 
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
  | LetRec(ts,e) ->
      fprintf fmt "@[<v>(@[<v 2>let rec ";
       pp_print_list 
          ~pp_sep:(fun fmt () -> fprintf fmt "@,and ") 
         pp_transition fmt ts;
      fprintf fmt " in (";
      pp_exp fmt e;
      fprintf fmt " : %a" print_ty (ty_of e);
      fprintf fmt "))@]"
  | Match(e,cases) -> 
    let pp_case fmt (c,xs,e) = 
      let pp_pat fmt = function
      | None,ty -> fprintf fmt "(_ : %a)" print_ty ty 
      | Some x,ty -> fprintf fmt "(%a : %a)" pp_ident x print_ty ty in
      let pp_pats fmt = function
      | [] -> ()
      | xs ->  
        pp_print_text fmt "(";
        pp_print_list 
         ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_pat fmt xs;
        pp_print_text fmt ")" in
      fprintf fmt "%s%a -> %a" c pp_pats xs pp_exp e;    
    in
    fprintf fmt "(@[<v>match (%a : %a) with" pp_exp e print_ty (ty_of e);
    pp_print_list 
      ~pp_sep:(fun fmt () -> fprintf fmt "@,| ") pp_case fmt cases;
    fprintf fmt ")@]"
  | Raise exc -> 
    (match exc with
    | Exception_Failure s -> 
        fprintf fmt "(raise (Failure \"%s\"))" s
    | Exception_Invalid_arg s -> 
        fprintf fmt "(raise (Invalid_arg \"%s\"))" s)
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
      fprintf fmt "array_length (%a : %a)"
        pp_exp e
        print_ty (ty_of e)
  | ListHd e ->
      fprintf fmt "list_hd (%a : %a)"
        pp_exp e
        print_ty (ty_of e)
  | ListTl e ->
      fprintf fmt "list_tl (%a : %a)"
        pp_exp e
        print_ty (ty_of e)
  | ArrayMapBy(n,x,e) ->
     fprintf fmt "array_map_by %d %s (%a : %a)" n x 
        pp_exp e
        print_ty (ty_of e)
  | ArrayFoldLeft(x,acc,e) ->
      fprintf fmt "array_fold_left %s (%a : %a) (%a : %a)" x 
        pp_exp acc 
        print_ty (ty_of acc) 
        pp_exp e
        print_ty (ty_of e)
  | ListFoldLeft(x,acc,e) ->
      fprintf fmt "list_fold_left %s (%a : %a) (%a : %a)" x 
        pp_exp acc 
        print_ty (ty_of acc) 
        pp_exp e
        print_ty (ty_of e)
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
