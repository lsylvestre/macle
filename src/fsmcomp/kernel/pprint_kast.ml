open Format
open Kast
open Ast

module PP_atom = struct
  
  open Atom
  
  let pp_ident = pp_print_text
  
  let pp_std_logic fmt v = 
    pp_print_text fmt @@
    match v with
    | U -> "'U'"
    | X -> "'X'"
    | Zero -> "'0'"
    | One -> "'1'"
    | Z -> "'Z'"
    | W -> "'W'"
    | L -> "'L'"
    | H -> "'H'"
    | Whatever -> "'-'"

  let pp_const fmt c =
    match c with 
    | Std_logic v -> 
        pp_std_logic fmt v
    | Bool b ->
        fprintf fmt "%b" b
    | Int n ->
        fprintf fmt "%d" n
    | EmptyList ->
        pp_print_text fmt "[]"
    | Unit ->
        pp_print_text fmt "()"

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
    | And -> "&&" 
    | Or -> "||"

  let pp_unop fmt p = 
    pp_print_text fmt @@
    match p with 
    | Not -> "not" 
    | Uminus -> "-"
    | DivBy2 -> "div_by_2"
    | Mod2 -> "mod2"

  let pp_op fmt = function
  | Unop p -> 
      pp_unop fmt p
  | Binop p -> 
      pp_binop fmt p
  | TyAnnot ty -> 
      fprintf fmt "(fun x -> (x : %a))" Ktypes.print_ty ty 
  | (FromCaml ty) -> 
      fprintf fmt "(#from_caml %a)" Ktypes.print_ty ty
  | (ToCaml ty) -> 
      fprintf fmt "(#to_caml %a)" Ktypes.print_ty ty 
  | CamlHeader ->
      pp_print_text fmt "#caml_header"
  | CamlComputedField ->
      pp_print_text fmt "#caml_computer_field"
  | Size_hd ->
      pp_print_text fmt "#size_hd"
  | CamlField i ->
      fprintf fmt "(#caml_field %d)" i

  let parenthesized ~paren fmt cb = 
    if paren then fprintf fmt "("; 
    cb (); 
    if paren then fprintf fmt ")" 

  let pp_atom fmt a = 
    let rec pp_atom_aux ~paren fmt a = 
    match a with
    | Var x ->
        pp_print_text fmt x
    | State q -> 
        pp_print_text fmt q
    | Const c -> 
        pp_const fmt c
    | Prim (Binop p,[a1;a2]) ->
        parenthesized ~paren fmt @@ fun () ->
          fprintf fmt "%a %a %a"
            (pp_atom_aux ~paren:true) a1
            pp_binop p
            (pp_atom_aux ~paren:true) a2
    | Prim (Unop p,[a]) -> 
        parenthesized ~paren fmt @@ fun () ->
          fprintf fmt "%a %a"
            pp_unop p
            (pp_atom_aux ~paren:true) a
    | Prim (TyAnnot t,[a]) ->
       fprintf fmt "(%a : %a)" 
          (pp_atom_aux ~paren:false) a 
          Ktypes.print_ty t
    | Prim(op,args) -> 
       pp_op fmt op;
       pp_print_text fmt "(";
       pp_print_list 
          ~pp_sep:(fun fmt () -> fprintf fmt ",") (pp_atom_aux ~paren:false) fmt args;
       pp_print_text fmt ")"
  in
  pp_atom_aux ~paren:false fmt a

end 

let pp_state = pp_print_text

module PP_VSML = struct
  open PP_atom
  open VSML

  let rec pp_exp fmt e = 
    match e with
    | Atom a -> 
        pp_atom fmt a
    | If (a,e1,e2) ->
      fprintf fmt "if %a@ then @[<hov>%a@]@ else @[<hov>%a@]@,"
        pp_atom a
        pp_exp e1
        pp_exp e2
    | Case (a,hs,e) ->
        let pp_handle fmt (c,e) = 
          fprintf fmt "%a -> %a" pp_const c pp_exp e 
        in
        fprintf fmt "case %a with" pp_atom a;
        pp_print_list 
          ~pp_sep:(fun fmt () -> fprintf fmt "@,| ") pp_handle fmt hs;
        fprintf fmt "otherwise %a" pp_exp e;
    | State(q,args) ->
        fprintf fmt "%s(" q;
        pp_print_list 
          ~pp_sep:(fun fmt () -> fprintf fmt ",@,") pp_atom fmt args;
         fprintf fmt ")@,"
    | LetIn(bs,e) -> 
      fprintf fmt "@[<v 2>let ";
      pp_print_list 
          ~pp_sep:(fun fmt () -> fprintf fmt "@, and ") 
           (fun fmt ((x,ty),fsm) -> 
               fprintf fmt "%s : %a =@,%a" x Ktypes.print_ty ty pp_fsm fsm) fmt bs;
       fprintf fmt "@]@, in %a" pp_exp e
    | DoIn(bs,e) -> 
      fprintf fmt "@[<v>do ";
      pp_print_list 
          ~pp_sep:(fun fmt () -> fprintf fmt "@, and ") 
           (fun fmt (x,a) -> 
               fprintf fmt "%s := %a" x pp_atom a) fmt bs;
       fprintf fmt "@]@, in %a" pp_exp e
    | Continue a -> 
       fprintf fmt "@[<v 2>continue %a@]" pp_atom a
    
  and pp_fsm fmt (ts,e) =
    fprintf fmt "(@[<v>let automaton@,";
       pp_print_list 
          ~pp_sep:(fun fmt () -> fprintf fmt "@,| ") 
         pp_transition fmt ts;
      fprintf fmt "    end in ";
      pp_exp fmt e;
      fprintf fmt ")@]"

  and pp_transition fmt ((q,xs),e) = 
    fprintf fmt "@[<hov 2> %s(@]" q; 
    pp_print_list 
      ~pp_sep:(fun fmt () -> fprintf fmt ", ") 
     (fun fmt (x,ty) ->  fprintf fmt "%s:%a" x Ktypes.print_ty ty) fmt xs;
    fprintf fmt ") -> @,";
    pp_exp fmt e;
    fprintf fmt "@]"

  and print_mode fmt = function
  | In -> pp_print_text fmt "input"
  | Out -> pp_print_text fmt "output"
  | Local -> pp_print_text fmt "local"

  let pp_circuit fmt {x;s;body} =
    fprintf fmt "@[<v 2>circuit %s sig " x;
    pp_print_list 
      ~pp_sep:(fun fmt () -> fprintf fmt ", ") 
      (fun fmt (d,x,ty) -> fprintf fmt "%a %s : %a" print_mode d x Ktypes.print_ty ty) fmt s;
    fprintf fmt " end =@,";
    pp_fsm fmt body;
    fprintf fmt "@]"
end
