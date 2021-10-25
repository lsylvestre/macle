open Kast
open ESML

open Format

let allow_heap_access = ref false
let allow_heap_assign = ref false

let caml_heap_base = "caml_heap_base"

let c_binop fmt p = 
  let open Atom in
  pp_print_text fmt @@
  match p with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | Eq -> "="
  | Neq -> "/="
  | And -> "and"
  | Or -> "or"

let c_unop fmt p = 
  let open Atom in
  pp_print_text fmt @@
  match p with
  | Not -> "not"
  | Uminus -> "-"
  | DivBy2 | Mod2 -> assert false

(* - two underscores can't be consecutive 
   - an identifier can't start with '_' 
   - an identifier can't contain a single quote

   - '#' character introduced before by Gensym.gensym 
     is replaced by the string "_id" *)
let vhdl_ident x =
  let open String in
  let x = concat "_0x" @@ split_on_char '#' x in
  let y = concat "CHAR_ASCII39" @@ split_on_char '\'' x in
  y |> split_on_char '_' 
  |> List.map (function "" -> "CHAR_ASCII95" | s -> s)
  |> concat "_"

let c_state fmt q = 
  pp_print_text fmt (vhdl_ident @@ String.uppercase_ascii q)

let c_ident fmt q = 
  pp_print_text fmt (vhdl_ident @@ String.lowercase_ascii q)

let pp_std_logic fmt v = 
  let open Atom in
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

let parenthesized ~paren fmt cb = 
  if paren then fprintf fmt "("; 
  cb (); 
  if paren then fprintf fmt ")" 

let is_ptr t = 
  let open Ktypes in
  match t with
  Ktypes.TPtr _ | TVar _ -> true
  | _ (* immediate value *) -> false

let c_const fmt c = 
  let open Atom in
  match c with
  | Std_logic v -> 
     pp_std_logic fmt v
  | Bool b -> 
    fprintf fmt "%b" b
  | Int n -> 
    fprintf fmt "to_signed(%d,31)" n
  | Unit -> 
    pp_print_text fmt "UNIT_VALUE"
  | EmptyList -> 
    pp_print_text 
      fmt 
      "X\"00000001\"" (* constant 0 as immediate OCaml value *)

let c_atom env fmt a = 
  let open Atom in
  let rec pp_atom ~paren fmt a =
    match a with 
    | State q -> 
        c_state fmt q
    | Var x ->
        (match List.assoc_opt x env with
        | Some a -> pp_atom ~paren fmt a
        | None -> c_ident fmt x)
    | Const c -> 
        c_const fmt c
    | Prim p -> 
     (match p with
      | (Binop p,[a1;a2]) ->
        parenthesized ~paren fmt @@ fun () ->
          let f = function Mul -> true | _ -> false in  
          fprintf fmt (if f p 
                       then "RESIZE((%a %a %a),31)" 
                       else "%a %a %a")
            (pp_atom ~paren:true) a1
            c_binop p
            (pp_atom ~paren:true) a2
     | (Unop DivBy2,[a]) -> 
        parenthesized ~paren fmt @@ fun () ->
          fprintf fmt "%a / 2"
            (pp_atom ~paren:true) a
     | (Unop Mod2,[a]) -> 
        parenthesized ~paren fmt @@ fun () ->
          fprintf fmt "(%a) mod 2"   (* todo *)
            (pp_atom ~paren:true) a
     | (Unop p,[a]) -> 
        parenthesized ~paren fmt @@ fun () ->
          fprintf fmt "%a %a"
            c_unop p
            (pp_atom ~paren:true) a
     | (CamlHeader,[addr]) -> 
          fprintf fmt "@[<hov>std_logic_vector(unsigned(%s) +@,unsigned(%a(19 downto 0)) - 4)@]"
              caml_heap_base
              (pp_atom ~paren:true) addr
     | (CamlField i,[addr]) -> 
          fprintf fmt "@[<hov>std_logic_vector(unsigned(%s) +@,unsigned(%a(19 downto 0)) + (%d))@]"
              caml_heap_base
              (pp_atom ~paren:true) addr
              (i*4)
     | (CamlComputedField,[addr;ofs]) -> 
          fprintf fmt "@[<hov>std_logic_vector(unsigned(%s) +@,unsigned(%a(19 downto 0)) +@,RESIZE(unsigned(%a(19 downto 0)) * 4,32))@]" (* au lieu de "* 4", utiliser [& "00"] *)
              caml_heap_base
              (pp_atom ~paren:true) addr       (* !!! que faire si addr n'est pas une variable *)
              (pp_atom ~paren:true) ofs
     | Size_hd,[header] ->
         fprintf fmt "signed(\"00000000000\"&%a(21 downto 2))" (* "std_logic_vector(%a)(21 downto 2)) ...??" *)
                 (pp_atom ~paren:true) header
     | (TyAnnot _,[a]) -> 
         pp_atom ~paren fmt a 
     | (FromCaml t,[a]) -> 
          if is_ptr t 
          then pp_atom ~paren fmt a 
          else fprintf fmt "signed(%a(31 downto 1))" (pp_atom ~paren:true) a
     | (ToCaml t,[a]) -> 
          if is_ptr t 
          then pp_atom ~paren fmt a 
          else fprintf fmt "std_logic_vector(%a)& \"1\"" (pp_atom ~paren:true) a
     | _ -> assert false) (* ill-formed primitive application *)
  in
  pp_atom ~paren:false fmt a

let assign env fmt x pp y = 
  fprintf fmt "%a <= %a;" c_ident x pp y

let rec c_exp env d fmt e = 
  let rec c_exp_aux env d fmt e =
  match e with
  | Atom a -> 
      assign env fmt d (c_atom env) a
  | DoThen(bs,e) ->
      (* the Quartus® II software supports multiple assignments 
         to the same signal even though the last one assigned takes 
         precedence.

          https://www.intel.com/content/www/us/en/support/programmable/articles/000085525.html
      *)
      fprintf fmt "@[<v>";
      List.iter (fun (x,a) ->
         assign env fmt x (c_atom env) a;
         fprintf fmt "@,") bs;
      c_exp (bs@env) d fmt e;
      fprintf fmt "@]";
  | If(a,e1,((If _) as e2)) ->
      fprintf fmt "%a then@,%a@]@,@[<v 2>elsif %a@]@,"
        (c_atom env) a 
        (c_exp env d) e1
        (c_exp_aux env d) e2
  | If(a,e1,e2) ->
      fprintf fmt "%a then@,%a@]@,@[<v 2>else@,%a@]@,end if;"
        (c_atom env) a 
        (c_exp env d) e1
        (c_exp env d) e2
  | Case(a,hs,e) ->
      let c_atom_case fmt a = 
        (match hs with 
        | ((Atom.Int _),_)::es -> fprintf fmt "to_integer(%a)" (c_atom env) a
        | _ -> c_atom env fmt a) in
      let c_const_case fmt = function
      | Atom.Int n -> fprintf fmt "%d" n
      | c -> c_const fmt c in
      let c_case_handler d fmt (c,e) = 
        fprintf fmt "@[<v 2>when %a =>@,%a@]" 
          c_const_case c
          (c_exp env d) e
      in
      fprintf fmt "@[<v 2>case %a is@,%a@,@[<v 2>when others =>@,%a@]@,end case;"
        c_atom_case a 
        (pp_print_list (c_case_handler d)) hs
        (c_exp env d) e
  in 
  match e with
  | If _ -> fprintf fmt "@[<v 2>if "; c_exp_aux env d fmt e
  | _ -> c_exp_aux env d fmt e 

let c_transition (s:state) fmt (q,e) = 
  fprintf fmt "@[<v 2>when %a =>@,%a@]" 
    c_state q
    (c_exp [] s) e

let c_transitions (s:state) fmt ts = 
  pp_print_list
    (c_transition s) fmt ts

let rec default_value fmt ty =
  let open Ktypes in
  match ty with
  | TConst TStd_logic -> pp_print_text fmt "'-'"
  | TConst TBool -> pp_print_text fmt "false"
  | TConst TInt -> pp_print_text fmt "to_signed(0,31)"
  | TConst TUnit -> pp_print_text fmt "UNIT_VALUE"
  | (TPtr _ | TVar _) -> 
      pp_print_text fmt "X\"00000000\""


let c_automaton ~reset ~clock (state_var:state) locals fmt (ts,e) =
  fprintf fmt "@[<v 2>process(%s,%s) begin@," reset clock;
  fprintf fmt "@[<v 2>if %s = '1' then@," reset;
  c_exp [] state_var fmt e; 
  fprintf fmt "@]@,";
  fprintf fmt "@[<v 2>elsif rising_edge(%s) then@," clock;
  fprintf fmt "@[<v 2>case %s is@," state_var;
  c_transitions state_var fmt ts;
  fprintf fmt "@,@]end case;@,@]";
  fprintf fmt "end if;@,@]";
  fprintf fmt "end process;@,"

let rec c_ty fmt ty = 
  let open Ktypes in
  match ty with
  | TConst TStd_logic -> pp_print_text fmt "std_logic"
  | TConst TBool -> pp_print_text fmt "boolean"
  | TConst TUnit -> pp_print_text fmt "unit"
  | TConst TInt -> fprintf fmt "@[<h>caml_int@]"
  | (TPtr _ | TVar _) -> 
       pp_print_text fmt "caml_value"

(* à mettre en argument de c_circuit *)
let outputs_initial_values = 
  Atom.[("rdy",Std_logic One)(* Zero ?*);("avm_rm_read",Std_logic Zero)]


let c_vars_decl d pp fmt vars =
  List.iter (fun (d',x,ty) -> if d' = d then pp fmt (x,ty)) vars

let c_var_local fmt (x,ty) = 
  fprintf fmt "signal %a : %a := %a;@," c_ident x 
      c_ty ty default_value ty

let c_var_in fmt (x,ty) = 
  fprintf fmt ";@,signal %a : in %a" c_ident x 
      c_ty ty

let c_var_out fmt (x,ty) = 
  fprintf fmt ";@,signal %a : out %a" c_ident x 
      c_ty ty

(* todo init default value *)
let compile_esml_circuit ?(reset="reset") ?(clock="clk")
             fmt ESML.{x=name;s=vars;body=automata } = 
  fprintf fmt "@[<v>library ieee;@,";
  fprintf fmt "use ieee.std_logic_1164.all;@,";
  fprintf fmt "use ieee.numeric_std.all;@,";
  fprintf fmt "use work.misc_%s.all;@,@," name;
  fprintf fmt "@[<v 2>entity %s is@," name;
  fprintf fmt "port(@[<v>signal %s : in std_logic;@," clock;
  fprintf fmt "signal %s : in std_logic" reset;
  
  c_vars_decl In c_var_in fmt vars;

  c_vars_decl Out c_var_out fmt vars;

  fprintf fmt ");@]@]@,";
  fprintf fmt "end entity;@,";
  fprintf fmt "@[<v 2>architecture RTL of %s is@," name;

  c_vars_decl Local c_var_local fmt vars;

  let state_vars = 
    List.map (fun _ -> Gensym.gensym "STATE") automata in
  
  List.iter2 (fun sv (ts,e)(* automaton *) ->
    let qs = List.map fst ts in
    fprintf fmt "@,type %s_T is (@[<hov>" sv;
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") c_state fmt qs;
    fprintf fmt ");@]@,";
    fprintf fmt "signal %s : %s_T;@," sv sv;
  )
    state_vars automata;
  fprintf fmt "@]@,";
  fprintf fmt "@[<v 2>begin@,";

   List.iter2 (fun st_reg a -> 
               c_automaton ~reset ~clock st_reg [] fmt a) 
    state_vars 
    automata;
  (* Misc.list_iter3 (fun st_reg locals a -> 
               c_automaton ~reset ~clock st_reg locals fmt a) 
    state_vars 
    l_locals 
    automata;*)
  


  fprintf fmt "@]@,end architecture;@]@."

