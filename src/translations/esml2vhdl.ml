open Esml

open Format

let allow_heap_access = ref false
let allow_heap_assign = ref false
let allow_trap = ref false

let caml_heap_base = "caml_heap_base"
let avm_rm_readdata = "avm_rm_readdata"



let array_defs = ref [] (* need order *)

module At_reset = struct

  (* set of variables *)
  module S = Set.Make(String)

  let rec w_inst (acc:S.t) (s:inst) : S.t =
    match s with
    | ESML_continue _ ->
        acc
    | ESML_do(bs,s) ->
        let acc' = List.fold_right (fun (x,_) acc -> S.add x acc) bs acc in
        w_inst acc' s
    | ESML_SetArray(_,s) ->
        w_inst acc s
    | ESML_if(a,s1,s2) ->
        let acc' = w_inst acc s1 in
        w_inst acc' s2

  (* [w_automaton a] compute the set of outputs and local
     variables assigned in the ESML automaton [a] *)
  let w_automaton (a:automaton) : S.t =
    let (ts,s) = a in
    let acc = List.fold_right (fun (_,s) acc -> w_inst acc s) ts S.empty in
    w_inst acc s

  let filter_signature (st:signature) (a:automaton) : signature =
    let acc = w_automaton a in
    List.filter (fun (_,x,_) -> S.mem x acc) st
end



let c_ty fmt (ty:Typ.t) : unit =
  let open Typ in
  let rec string_of_ty = function
  | TConst TStd_logic -> "std_logic"
  | TConst TBool -> "boolean"
  | TConst TUnit -> "unit"
  | TConst TInt -> "caml_int"
  | (TPtr _ | TVar _) ->
       "caml_value"
  | TFlatArray (ty,size) ->
    let sty = string_of_ty ty in
    let s = "array_" ^ sty ^ "_" ^ string_of_int size in
    if not @@ List.mem_assoc s !array_defs
    then array_defs := (s,(sty,size))::!array_defs;
    s
  in
  pp_print_text fmt (string_of_ty ty)



let c_binop fmt (p:Atom.binop) : unit =
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

let c_unop fmt (p:Atom.unop) : unit =
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
let vhdl_ident (x:ident) : ident =
  let open String in
  let x = concat "_0x" @@ split_on_char '#' x in
  let y = concat "CHAR_ASCII39" @@ split_on_char '\'' x in
  y |> split_on_char '_'
  |> List.map (function "" -> "CHAR_ASCII95" | s -> s)
  |> concat "_"

let c_cstr fmt (c:ident) : unit =
  pp_print_text fmt (vhdl_ident @@ String.uppercase_ascii c)

let c_state = c_cstr

let c_ident fmt (q:ident) : unit =
  pp_print_text fmt (vhdl_ident @@ String.lowercase_ascii q)

let pp_std_logic fmt (v:Atom.std_logic) : unit =
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

let parenthesized ~paren fmt (cb:unit -> unit) : unit =
  if paren then fprintf fmt "(";
  cb ();
  if paren then fprintf fmt ")"

let is_ptr (t:Typ.t) : bool =
  let open Typ in
  match t with
  TPtr _ | TVar _ -> true
  | _ (* immediate value *) -> false

let c_const fmt (c:Atom.const) : unit =
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
  | Cstr c ->
      (* (match c with
      | "[]" -> pp_print_text fmt "X\"00000001\""
      | "::" -> pp_print_text fmt "to_signed(0,31)"
      | "," -> pp_print_text fmt "to_signed(0,31)"
      | _ -> *)
      let (n,tys) = Ast.val_of_constructor c in
      if tys = []
      then fprintf fmt "std_logic_vector(to_signed(%d,31)) & \"1\"" n
      else fprintf fmt "to_signed(%d,31)" n

  | EmptyList ->
      pp_print_text fmt
        "X\"00000001\"" (* constant 0 as immediate OCaml value *)

let c_atom env fmt (a:Atom.t) : unit =
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
     | (SizeHeader,[a]) ->
        fprintf fmt "size_header(%a)" (pp_atom ~paren:true) a
     | (TagHd,[a]) ->
        fprintf fmt "tag_header(%a)" (pp_atom ~paren:true) a
     | (IsImm,[a]) ->
        fprintf fmt "is_imm(%a)" (pp_atom ~paren:true) a
     | (FromCaml t,[a]) ->
         (match t with
         | TConst TBool -> fprintf fmt "(%a(1) = '1')" (pp_atom ~paren:true) a
         | TConst TInt -> fprintf fmt "signed(%a(31 downto 1))" (pp_atom ~paren:true) a
         | _ -> pp_atom ~paren fmt a)
     | (ToCaml t,[a]) ->
         (match t with (* à vérifier *)
         | TConst TBool -> fprintf fmt "BOOLEAN_TO_STD_LOGIC(%a)& \"1\"" (pp_atom ~paren:true) a  (* todo *)
         | TConst TInt -> fprintf fmt "std_logic_vector(%a)& \"1\"" (pp_atom ~paren:true) a
         | _ -> pp_atom ~paren fmt a)
     | (ComputeAddress,[h;addr;ofs]) ->
         fprintf fmt "compute_address(%a, %a, %a)"
           (pp_atom ~paren:false) h
           (pp_atom ~paren:false) addr
           (pp_atom ~paren:false) ofs
     | (FlatArrayMake ty,es) ->
         fprintf fmt "@[<v 2>%a_create (@," c_ty ty;
         pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") (pp_atom ~paren:false) fmt es;
         fprintf fmt ")@]"
     | (FlatArrayGet,[a;Const (Int i)]) ->
          fprintf fmt "%a(%d)" (pp_atom ~paren:false) a i
     | (FlatArrayGet,[a;i]) ->
          fprintf fmt "%a(to_integer(%a))"
            (pp_atom ~paren:false) a
            (pp_atom ~paren:false) i
     | Array_create n,[a] ->
          fprintf fmt "(others => %a)" (pp_atom ~paren:false) a
     | NextField,[a] ->
          fprintf fmt "std_logic_vector(unsigned(%a) + to_unsigned(4,31))"
             (pp_atom ~paren:false) a
     | _ -> assert false) (* ill-formed primitive application *)
  in
  pp_atom ~paren:false fmt a

let assign env fmt (x:ident) pp y =
  fprintf fmt "%a <= %a;" c_ident x pp y

let rec c_instruction env (dst:ident) fmt (s:inst) : unit =
  match s with
  | ESML_continue q ->
      assign env fmt dst c_state q
  | ESML_do(bs,s) ->
      (* "the Quartus II software supports multiple assignments
          to the same signal even though the last one assigned takes
          precedence."

          https://www.intel.com/content/www/us/en/support/programmable/articles/000085525.html
      *)
      fprintf fmt "@[<v>";
      List.iter (fun (x,a) ->
                   assign env fmt x (c_atom env) a;
                   fprintf fmt "@,")
          bs;
      c_instruction (bs@env) dst fmt s;
      fprintf fmt "@]";
  | ESML_SetArray((x,idx,a),s) ->
      fprintf fmt "@[%a(to_integer(%a)) <= %a;@,%a@]"
        c_ident x
        (c_atom env) idx
        (c_atom env) a
        (c_instruction env dst) s
  | ESML_if(a,s1,s2) ->
      fprintf fmt "@[<v 2>if %a then@,%a@]@,@[<v 2>else@,%a@]@,end if;"
        (c_atom env) a
        (c_instruction env dst) s1
        (c_instruction env dst) s2


let c_transition (dst:state) fmt ((q,s) : transition) : unit =
  fprintf fmt "@[<v 2>when %a =>@,%a@]"
    c_state q
    (c_instruction [] dst) s

let c_transitions (dst:state) fmt (ts:transition list) : unit =
  pp_print_list
    (c_transition dst) fmt ts

let rec default_value fmt (ty:Typ.t) : unit =
  let open Typ in
  match ty with
  | TConst TStd_logic ->
      pp_print_text fmt "'0'"
  | TConst TBool ->
      pp_print_text fmt "false"
  | TConst TInt ->
      pp_print_text fmt "to_signed(0,31)"
  | TConst TUnit ->
      pp_print_text fmt "UNIT_VALUE"
  | (TPtr _ | TVar _) ->
      pp_print_text fmt "X\"00000000\""
  | TFlatArray (ty,n) ->
       fprintf fmt "(others=>%a)" default_value ty

let outputs_initial_values =
  let open Atom in
  [ ("rdy",Std_logic One);
    ("avm_rm_read",Std_logic Zero) ]

let c_automaton ~(reset:ident) ~(clock:ident) ?(at_reset=[]) (state_var:state) fmt (ts,e) =
  let state_var = vhdl_ident state_var in
  fprintf fmt "@[<v 2>process(%s,%s) begin@," reset clock;
  fprintf fmt "@[<v 2>if %s = '1' then@," reset;

  List.iter (fun (_,x,ty) ->
     match List.assoc_opt x outputs_initial_values with
     | None ->
        fprintf fmt "%a <= %a;@," c_ident x default_value ty
     | Some a ->
        fprintf fmt "%a <= %a;@," c_ident x c_const a
  ) at_reset;

  c_instruction [] state_var fmt e;
  fprintf fmt "@]@,";
  fprintf fmt "@[<v 2>elsif rising_edge(%s) then@," clock;
  fprintf fmt "@[<v 2>case %s is@," state_var;
  c_transitions state_var fmt ts;
  fprintf fmt "@,@]end case;@,@]";
  fprintf fmt "end if;@,@]";
  fprintf fmt "end process;@,"


let c_vars_decl d pp fmt vars =
  List.iter (fun (d',x,ty) -> if d' = d then pp fmt (x,ty)) vars

let c_var_local fmt (x,ty) =
  fprintf fmt "signal %a : %a;@," c_ident x c_ty ty

let c_var_in fmt (x,ty) =
  fprintf fmt ";@,signal %a : in %a" c_ident x
      c_ty ty

let c_var_out fmt (x,ty) =
  fprintf fmt ";@,signal %a : out %a" c_ident x
      c_ty ty



let compile_esml_circuit ?(reset="reset") ?(clock="clk")
             fmt {x=name;vars=st;body=automata } =

  fprintf fmt "@[<v>library ieee;@,";
  fprintf fmt "use ieee.std_logic_1164.all;@,";
  fprintf fmt "use ieee.numeric_std.all;@,";
  fprintf fmt "use work.misc_%s.all;@,@," name;
  fprintf fmt "@[<v 2>entity %s is@," name;
  fprintf fmt "port(@[<v>signal %s : in std_logic;@," clock;
  fprintf fmt "signal %s : in std_logic" reset;

  c_vars_decl In c_var_in fmt st;

  c_vars_decl Out c_var_out fmt st;

  fprintf fmt ");@]@]@,";
  fprintf fmt "end entity;@,";
  fprintf fmt "@[<v 2>architecture RTL of %s is@," name;

  c_vars_decl Local c_var_local fmt st;

  let state_vars =
    List.map (fun _ -> Gensym.gensym "STATE") automata in

  List.iter2 (fun sv (ts,e)(* automaton *) ->
    let sv = vhdl_ident sv in
    let qs = List.map fst ts in
    fprintf fmt "@,type %s_T is (@[<hov>"  sv;
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") c_state fmt qs;
    fprintf fmt ");@]@,";
    fprintf fmt "signal %s : %s_T;@," sv sv;
  )
    state_vars automata;
  fprintf fmt "@]@,";
  fprintf fmt "@[<v 2>begin@,";


  let w_vars = List.map (At_reset.filter_signature st) automata in

  Misc.list_iter3(fun (q_var:ident) a at_reset ->
               c_automaton ~reset ~clock ~at_reset q_var fmt a)
    state_vars
    automata
    w_vars;

  fprintf fmt "@]@,end architecture;@]@."
