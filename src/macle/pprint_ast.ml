open Format
open Ast

let pp_ident = pp_print_text

let pp_const fmt c =
  match c with
  | Bool b ->
      fprintf fmt "%b" b
  | Int n ->
      fprintf fmt "%d" n
  | EmptyList ->
      pp_print_text fmt "[]"
  | Unit ->
      pp_print_text fmt "()"
  | Cstr c ->
      pp_print_text fmt c

let pp_binop fmt p =
  pp_print_text fmt @@
  match p with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Le -> "<="
  | Ge -> ">="
  | Lt -> "<"
  | Gt -> ">"
  | Eq -> "="
  | Neq -> "<>"

let pp_unop fmt p =
  pp_print_text fmt @@
  match p with
  | Not -> "not"
  | Uminus -> "-"
  | DivBy2 -> "div_by_2"
  | Mod2 -> "mod2"

module PP_MACLE = struct
  open MACLE

  let rec pp_exp fmt (e,_) =
    match e with
    | Var x ->
        pp_print_text fmt x
    | Const c ->
        pp_const fmt c
    | Unop(p,e) ->
        fprintf fmt "(%a %a)"
          pp_unop p
          pp_exp e
    | Binop(p,e1,e2)->
        fprintf fmt "(%a %a @[<v>%a@])"
          pp_exp e1
          pp_binop p
          pp_exp e2
    | If (e1,e2,e3) ->
        fprintf fmt "if %a@ then @[<hov>%a@]@ else @[<hov>%a@]@,"
          pp_exp e1
          pp_exp e2
          pp_exp e3
    | App (q,es) ->
        fprintf fmt "@[<hov>%s(" q;
        pp_print_list
          ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp_exp fmt es;
        fprintf fmt ")@]"
    | Let(bs,e) ->
        fprintf fmt "@[<v>let ";
        pp_print_list
          ~pp_sep:(fun fmt () -> fprintf fmt "@,and ")
          (fun fmt ((x,_),e) ->
             fprintf fmt "@[<v 2>%s =@,%a@]" x pp_exp e) fmt bs;
       fprintf fmt "@,in@,%a@]" pp_exp e
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
        fprintf fmt "@,in ";
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
       | Ref e -> 
           fprintf fmt "(ref %a)"
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
           fprintf fmt "(array_length %a)"
             pp_exp e
       | ArrayMake{size;e} ->
           fprintf fmt "(array_make %a %a)"
             pp_exp size
             pp_exp e
        )
    | PacketPrim c ->
        (match c with
         | PkMake es ->
             fprintf fmt "#[|";
             pp_print_list
               ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_exp fmt es;
             fprintf fmt "|]";
         | PkGet (e,idx) ->
             fprintf fmt "(%a)[%a]" pp_exp e pp_exp idx
         | PkSet(x,idx,e') ->
             fprintf fmt "((%a)[%a] := %a)" pp_ident x pp_exp idx pp_exp e'
         | ToPacket(e,idx,n) ->
             fprintf fmt "(array_sub %a %a %d)" pp_exp e pp_exp idx n
         | OfPacket(e1,e2,idx,n) ->
             fprintf fmt "(array_blit %a %a %a %d)" 
               pp_exp e1 
               pp_exp e2 
               pp_exp idx n
         | PkMap((xs,e),es) ->
             fprintf fmt "(map (fun ";
             List.iter (fun (x,_) -> fprintf fmt "%s " x) xs;
             fprintf fmt "-> %a)" pp_exp e;
             List.iter (fun e -> fprintf fmt "%a " pp_exp e) es;
             fprintf fmt ")"
         | PkReduce(((acc,_),(y,_),e0),init,e) ->
             fprintf fmt "(reduce (fun %s %s -> %a) %a %a)" acc y
               pp_exp e0
               pp_exp init
               pp_exp e
         | PkScan(((acc,_),(y,_),e0),init,e) ->
             fprintf fmt "(scan (fun %s %s -> %a) %a %a)" acc y
               pp_exp e0
               pp_exp init
               pp_exp e)
    | Macro c ->
      (match c with
       | LazyOr(e1,e2) ->
           fprintf fmt "(%a || %a)" pp_exp e1 pp_exp e2
       | LazyAnd(e1,e2) ->
           fprintf fmt "(%a && %a)" pp_exp e1 pp_exp e2
       | OCamlArrayMap(n,f,e1,e2) ->
           fprintf fmt "array_map<%d> %s %a %a" n f
             pp_exp e1
             pp_exp e2
       | OCamlArrayReduce(n,f,init,e) ->
           fprintf fmt "array_reduce<%d> %s %a %a" n f
             pp_exp init
             pp_exp e
       | OCamlArrayScan(n,f,init,src,dst) ->
           fprintf fmt "array_scan<%d> %s %a %a %a" n f
             pp_exp init
             pp_exp src
             pp_exp dst)
   | StackPrim(c) ->
        (match c with
        | LetPop(xs,e) ->
            fprintf fmt "(";
            List.iter (fun (x,_) -> fprintf fmt "let %s = pop in@," x) xs;
            pp_exp fmt e;
            fprintf fmt ")"
        | Push(xs,e) ->
            fprintf fmt "(push [";
            List.iter (fun (x,_) -> fprintf fmt "%s, " x) xs;
            fprintf fmt "] then %a)" pp_exp e
        | Push_arg(e1,e2) ->
            fprintf fmt "(push [%a] then %a)" pp_exp e1 pp_exp e2
        | Save(q,e) ->
            fprintf fmt "(save %s then %a)" q pp_exp e
        | Restore ->
            fprintf fmt "(restore ())")

    and pp_transition fmt ((q,xs),e) =
      fprintf fmt "%s " q;
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt " ")
        pp_print_text fmt (List.map fst xs);
      fprintf fmt " = @,";
      pp_exp fmt e

    let pp_circuit fmt {x;xs;e} =
      fprintf fmt "@[<v>@[<v 2>circuit %s " x;
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt ", ")
        pp_print_text fmt (List.map fst xs);
      fprintf fmt " =@,";
      pp_exp fmt e;
      fprintf fmt "@] ;;@,@,@]"
  end


  module PP_TMACLE = struct
    open TMACLE
    open Types

    let rec pp_exp fmt (e,ty) =
      match e with
      | Var x ->
          pp_print_text fmt x
      | Const c ->
          pp_const fmt c
      | Unop(p,e) ->
          fprintf fmt "(%a %a)"
            pp_unop p
            pp_exp e
      | Binop (p,e1,e2) ->
          fprintf fmt "(%a %a @[<v>%a@])"
            pp_exp e1
            pp_binop p
            pp_exp e2
    | If (e1,e2,e3) ->
        fprintf fmt "if %a@ then @[<hov>%a@]@ else @[<hov>%a@]"
          pp_exp e1
          pp_exp e2
          pp_exp e3
    | App (q,es) ->
        fprintf fmt "@[<hov>%s(" q;
        pp_print_list
          ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp_exp fmt es;
        fprintf fmt ")@]"
    | Let(bs,e) ->
        fprintf fmt "@[<v>let ";
        pp_print_list
          ~pp_sep:(fun fmt () -> fprintf fmt "@,and ")
          (fun fmt ((x,ty),e) ->
             fprintf fmt "@[<v 2>%s : %a =@,%a@]" x print_ty ty pp_exp e) fmt bs;
         fprintf fmt "@,in@,%a@]" pp_exp e
    | LetFun(t,e) ->
        fprintf fmt "@[<v>(let ";
        pp_transition fmt t;
        fprintf fmt "in ";
        pp_exp fmt e;
        fprintf fmt ")@]"
    | LetRec(ts,e) ->
        fprintf fmt "@[<v>(@[<v>@[<v 2>let rec ";
        pp_print_list
          ~pp_sep:(fun fmt () -> fprintf fmt "@,and ")
          pp_transition fmt ts;
        fprintf fmt "@]@,in (";
        pp_exp fmt e;
        fprintf fmt " : %a" print_ty (ty_of e);
        fprintf fmt "))@]@]"
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
       | Ref e ->
          fprintf fmt "(ref (%a : %a))"
            pp_exp e print_ty (ty_of e)
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
           fprintf fmt "(array_length (%a : %a))"
             pp_exp e
             print_ty (ty_of e)
       | ArrayMake{size;e} ->
           fprintf fmt "(array_make %a (%a : %a))"
             pp_exp size
             pp_exp e
             print_ty (ty_of e)
      )
    | PacketPrim c ->
        (match c with
         | PkMake es ->
             fprintf fmt "#[|";
             pp_print_list
               ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_exp fmt es;
             fprintf fmt "|]";
         | PkGet (e,idx) ->
             fprintf fmt "(%a)[%a]" pp_exp e pp_exp idx
         | PkSet(x,idx,e') ->
             fprintf fmt "((%a)[%a] := %a)" pp_ident x pp_exp idx pp_exp e'
         | ToPacket(e,idx,n) ->
             fprintf fmt "(array_sub %a %a %d)" pp_exp e pp_exp idx n
         | OfPacket(e1,e2,idx,n) ->
             fprintf fmt "(array_blit %a %a %a %d)"
               pp_exp e1
               pp_exp e2
               pp_exp idx n
         | PkMap((xs,e),es) ->
             fprintf fmt "(map (fun ";
             List.iter (fun (x,_) -> fprintf fmt "%s " x) xs;
             fprintf fmt "-> %a)" pp_exp e;
             List.iter (fun e -> fprintf fmt "%a " pp_exp e) es;
             fprintf fmt ")"
         | PkReduce(((acc,_),(y,_),e0),init,e) ->
             fprintf fmt "(reduce (fun %s %s -> %a) %a %a" acc y
               pp_exp e0
               pp_exp init
               pp_exp e;
             fprintf fmt ")"
         | PkScan(((acc,_),(y,_),e0),init,e) ->
             fprintf fmt "(scan (fun %s %s -> %a) %a %a" acc y
               pp_exp e0
               pp_exp init
               pp_exp e;
             fprintf fmt ")")
    | Macro c ->
        (match c with
         | LazyOr(e1,e2) ->
             fprintf fmt "(%a || %a)" pp_exp e1 pp_exp e2
         | LazyAnd(e1,e2) ->
             fprintf fmt "(%a && %a)" pp_exp e1 pp_exp e2
         | OCamlArrayMap(n,f,src,dst) ->
             fprintf fmt "array_map<%d> %s (%a : %a) (%a : %a)" n f
               pp_exp src
               print_ty (ty_of src)
               pp_exp dst
               print_ty (ty_of dst)
         | OCamlArrayReduce(n,f,acc,e) ->
             fprintf fmt "array_reduce<%d> %s (%a : %a) (%a : %a)" n f
               pp_exp acc
               print_ty (ty_of acc)
               pp_exp e
               print_ty (ty_of e)
         | OCamlArrayScan(n,f,acc,e,res) ->
             fprintf fmt "array_reduce<%d> %s (%a : %a) (%a : %a) (%a : %a)" n f
               pp_exp acc
               print_ty (ty_of acc)
               pp_exp e
               print_ty (ty_of e)
               pp_exp res
               print_ty (ty_of res))
    | StackPrim(c) ->
        (match c with
        | LetPop(xs,e) ->
            fprintf fmt "(";
            List.iter (fun (x,_) -> fprintf fmt "let %s = pop in@," x) xs;
            pp_exp fmt e;
            fprintf fmt ")"
        | Push(xs,e) ->
            fprintf fmt "(push [";
            List.iter (fun (x,_) -> fprintf fmt "%s, " x) xs;
            fprintf fmt "] then %a)" pp_exp e
        | Push_arg(e1,e2) ->
            fprintf fmt "(push [%a] then %a)" pp_exp e1 pp_exp e2
        | Save(q,e) ->
            fprintf fmt "(save %s then %a)" q pp_exp e
        | Restore ->
            fprintf fmt "(restore ())")


  and pp_transition fmt ((q,xs),e) =
    fprintf fmt "%s (" q;
    pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt ", ")
      pp_tyconstr fmt xs;
    fprintf fmt ") = @,";
    pp_exp fmt e

  and pp_tyconstr fmt (x,ty) =
    fprintf fmt "%s : %a" x print_ty ty

  let pp_circuit fmt {x;xs;e} =
    fprintf fmt "@[<v>@[<v 2>circuit %s (" x;
    pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_tyconstr fmt xs;
    fprintf fmt ") : %a =@," print_ty (ty_of e);
    pp_exp fmt e;
    fprintf fmt "@] ;;@,@,@]"
end
