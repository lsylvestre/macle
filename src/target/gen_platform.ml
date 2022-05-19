open Format
open Esml2vhdl

open Esml

  (* ***************************************** *)

let flag_print_compute_time = ref false
let flag_print_compute_time_short = ref false

let mk_package name fmt =
  fprintf fmt "@[<v>library ieee;@,";
  fprintf fmt "use ieee.std_logic_1164.all;@,";
  fprintf fmt "use ieee.numeric_std.all;@,@,";
  fprintf fmt "@[<v 2>package misc_%s is@," name;
  fprintf fmt "type UNIT is (UNIT_VALUE);@,";
  fprintf fmt "subtype caml_value is std_logic_vector(31 downto 0);@,";
  fprintf fmt "subtype caml_ptr is std_logic_vector(31 downto 0);@,";
  fprintf fmt "subtype caml_int is signed(30 downto 0);@,";


  let array_defs_list = List.rev !array_defs in

  List.iter (fun (name,(sty,n)) ->
       fprintf fmt "type %s is array (0 to %d) of %s;@," name (n-1) sty) array_defs_list;

List.iter (fun (name,(sty,n)) ->
       fprintf fmt "@[<v 2>function %s_create(" name;
        for i = 1 to n-1 do
          fprintf fmt "x%d," i
        done;
        fprintf fmt "x%d" n;
        fprintf fmt ":%s) return %s;@," sty name
      ) array_defs_list;


  fprintf fmt "function bool_to_std_logic(X : boolean) return std_logic;@,";

  fprintf fmt "function if_int(c : boolean; n : caml_int; m : caml_int) return caml_int;@,";

  fprintf fmt "@[<v 2>function compute_address(@[<v>heap_base : caml_value;@,";
  fprintf fmt "address : caml_value;@,";
  fprintf fmt "offset : caml_int) return caml_value;@]@]@,";

  fprintf fmt "function size_header(x : caml_value) return caml_int;@,";

  fprintf fmt "function tag_header(x : caml_value) return caml_int;@,";

  fprintf fmt "function is_imm(x : caml_value) return boolean;@,";

  fprintf fmt "@]@,end;@,";
  fprintf fmt "@[<v 2>package body misc_%s is@," name;

  List.iter (fun (name,(sty,n)) ->
        fprintf fmt "@[<v 2>function %s_create(" name;
        for i = 1 to n-1 do
          fprintf fmt "x%d," i
        done;
        fprintf fmt "x%d" n;
        fprintf fmt ":%s) return %s is@," sty name;
        fprintf fmt "begin@,";
        fprintf fmt " return (";
        for i = 1 to n-1 do
          fprintf fmt "x%d," i
        done;

        if n = 1 (* LRM (section 9.3.3 Aggregates) states:
                    "Aggregates containing a single element association
                     shall always be specified using named association
                     in order to distinguish them from parenthesized expressions."
                     *)
        then fprintf fmt "0 => ";

        fprintf fmt "x%d);@," n;
        fprintf fmt "end;@,";
     ) array_defs_list;


  fprintf fmt "@[<v 2>function bool_to_std_logic(X : boolean) return std_logic is@,";
  fprintf fmt "@[<v 2>begin@,";
  fprintf fmt "@[<v 2>if X then@,";
  fprintf fmt "return '1';@]@,";
  fprintf fmt "@[<v 2>else@,";
  fprintf fmt "return '0';@]@,";
  fprintf fmt "end if;@]@,";
  fprintf fmt "end;@]@,";

  fprintf fmt "function if_int(c : boolean; n : caml_int; m : caml_int) return caml_int is@,";
 fprintf fmt "@[<v 2>begin@,";
  fprintf fmt "@[<v 2>if c then@,";
  fprintf fmt "return n;@]@,";
  fprintf fmt "@[<v 2>else@,";
  fprintf fmt "return m;@]@,";
  fprintf fmt "end if;@]@,";
  fprintf fmt "end;@]@,";

  fprintf fmt "@[<v 2>function compute_address(@[<v>heap_base : caml_value;@,";
  fprintf fmt "address : caml_value;@,";
  fprintf fmt "offset : caml_int) return caml_value is@]@,";
  fprintf fmt "@[<v 2>begin@,";
  fprintf fmt "return @[<v 2>std_logic_vector(@,@[<v>unsigned(heap_base) +@,";
  fprintf fmt "unsigned(address(19 downto 0)) +@,";
  fprintf fmt "(unsigned(offset(19 downto 0)) & \"00\")@]@,);@]@]@,";
  fprintf fmt "end;@]@,";

  fprintf fmt "@[<v 2>function size_header(x : caml_value) return caml_int is@,";
  fprintf fmt "variable r : std_logic_vector(30 downto 0);@,";
  fprintf fmt "@[<v 2>begin@,";
  fprintf fmt "r := \"00000000000\" & x(21 downto 2);@,";
  fprintf fmt "return signed(r);@]@,";
  fprintf fmt "end;@]@]@,";

  fprintf fmt "@[<v 2>function tag_header(x : caml_value) return caml_int is@,";
  fprintf fmt "variable r : std_logic_vector(30 downto 0);@,";
  fprintf fmt "@[<v 2>begin@,";
  fprintf fmt "r := \"00000000000000000000000\" & x(31 downto 24);@,";
  fprintf fmt "return signed(r);@]@,";
  fprintf fmt "end;@]@]@,";

  fprintf fmt "function is_imm(x : caml_value) return boolean is@,";
  fprintf fmt "@[<v 2>begin@,";
  fprintf fmt "return (x(0) = '1');@]@,";
  fprintf fmt "end;@]@]@,";

  fprintf fmt "end;@,@."

  (* ***************************************** *)


let bin_of_int ?(pad=4) d =
  if d < 0 || pad < 1 then invalid_arg "bin_of_int" else
  let open Bytes in
  let b = make pad '0' in
  let rec aux d i =
    if d >= 0 && i >= 0 then
      (set b i (Char.chr ((d land 1) + Char.code '0'));
       aux (d lsr 1) (i-1))
    else ()
  in
  aux d (pad-1);
  to_string b

let conversion_from_vect ty fmt x =
  let open Typ in
  match ty with
  | TConst TStd_logic ->
      fprintf fmt "%s(0)" x
  | TConst TBool ->
      fprintf fmt "(%s(0)) = '1'" x
  | TConst TInt ->
      fprintf fmt "signed(%s(30 downto 0))" x
  | TConst TUnit ->
      pp_print_text fmt "UNIT_VALUE"
  | (TPtr _| TVar _) ->  pp_print_text fmt x(* assert false (?) *)
  | TPacket _ ->
      assert false  (* packets cannot be converted into OCaml word *)

let set_result dst ty fmt x =
  let open Typ in
  match ty with
  | TConst TStd_logic ->
      fprintf fmt "%s <= X\"0000000\" & \"000\" & %s" dst x
  | TConst TBool ->
      fprintf fmt "@[<v 2>if %s then@," x;
      fprintf fmt "%s <= X\"00000001\";@]@," dst;
      fprintf fmt "@[<v 2>else@,%s <= X\"00000000\";@]@,end if" dst;
  | TConst TInt ->
      fprintf fmt "%s <= \"0\" & std_logic_vector(%s)" dst x
  | TConst TUnit ->
      fprintf fmt "%s <= X\"00000001\"" dst
  | (TPtr _ | TVar _) -> fprintf fmt "%s <= %s" dst x
  | TPacket _ ->
      assert false (* packets cannot be returned as result *)

let gen_cc fmt (envi,envo,_) (envi',envo',_) name =
  (* assume : ("start",_) in envi && ("rdy",_) in envo *)
  let envi = List.filter (fun (x,_) -> x <> "start") envi in
  let envo = List.filter (fun (x,_) -> x <> "rdy") envo in

  fprintf fmt "@[<v>-- AVALON MM-slave wrapper around the core %s IP@," name;
  fprintf fmt "library IEEE;@,";
  fprintf fmt "use IEEE.std_logic_1164.all;@,";
  fprintf fmt "use IEEE.numeric_std.all;@,";
  fprintf fmt "use work.misc_%s.all;@,@," name;
  fprintf fmt "@[<v 2>entity avs_%s is@," name;
  fprintf fmt "port (@[< v>";
  fprintf fmt "@[<v 2>avs_s0_address : in std_logic_vector(3 downto 0)  := (others => '0');@,";
  fprintf fmt "-- 0000  : control/status register (b1=start, b0=rdy)@,";
  let regs = envi'@envo' in
  let len = List.length regs in
  List.iteri (fun i (x,t) ->
    fprintf fmt "-- %s  : %s register@," (bin_of_int (i+1)) x) regs;

  (if !allow_heap_access || !allow_heap_assign then
    fprintf fmt "-- %s  : caml_heap_base register@," (bin_of_int (len+1)));

  fprintf fmt "@]@,avs_s0_read        : in  std_logic                     := '0';@,";
  fprintf fmt "avs_s0_readdata    : out std_logic_vector(31 downto 0);@,";
  fprintf fmt "avs_s0_write       : in  std_logic                     := '0';@,";
  fprintf fmt "avs_s0_writedata   : in  std_logic_vector(31 downto 0) := (others => '0');@,";
  fprintf fmt "clock_clk          : in  std_logic                     := '0';@,";
  fprintf fmt "reset_reset        : in  std_logic                     := '0'";

  if !allow_heap_access then begin
    fprintf fmt ";@,@,-- READ MASTER INTERFACE@,";
    fprintf fmt "avm_rm_address   : out std_logic_vector(31 downto 0);@,";
    fprintf fmt "avm_rm_read      : out std_logic;@,";
    fprintf fmt "avm_rm_readdata  : in std_logic_vector(31 downto 0);@,";
    fprintf fmt "avm_rm_waitrequest : in std_logic@,";
  end;

  if !allow_heap_assign then begin
    fprintf fmt ";@,@,-- WRITE MASTER INTERFACE@,";
    fprintf fmt "avm_wm_address   : out std_logic_vector(31 downto 0);@,";
    fprintf fmt "avm_wm_write      : out std_logic;@,";
    fprintf fmt "avm_wm_writedata  : out std_logic_vector(31 downto 0);@,";
    fprintf fmt "avm_wm_waitrequest : in std_logic@,";
  end;

  fprintf fmt ");@]@]@,";
  fprintf fmt "end entity;@,@,";
  fprintf fmt "@[<v 2>architecture rtl of avs_%s is@," name;
  fprintf fmt "@[<v 2>component %s is@," name;
  fprintf fmt "port (@[< v>";
  fprintf fmt "signal clk : in std_logic;@,";
  fprintf fmt "signal reset : in std_logic;@,";
  fprintf fmt "signal start : in std_logic;@,";
  fprintf fmt "signal rdy : out std_logic;@,";

  if !allow_heap_access || !allow_heap_assign then begin
    fprintf fmt "signal caml_heap_base   : in std_logic_vector(31 downto 0);@,";
  end;

  if !allow_heap_access then begin
    fprintf fmt "signal avm_rm_address   : out std_logic_vector(31 downto 0);@,";
    fprintf fmt "signal avm_rm_read      : out std_logic;@,";
    fprintf fmt "signal avm_rm_readdata  : in std_logic_vector(31 downto 0);@,";
    fprintf fmt "signal avm_rm_waitrequest : in std_logic;@,";
  end;

  if !allow_heap_assign then begin
    fprintf fmt "signal avm_wm_address   : out std_logic_vector(31 downto 0);@,";
    fprintf fmt "signal avm_wm_write      : out std_logic;@,";
    fprintf fmt "signal avm_wm_writedata  : out std_logic_vector(31 downto 0);@,";
    fprintf fmt "signal avm_wm_waitrequest : in std_logic;@,";
  end;

  List.iter (fun (x,t) -> fprintf fmt "signal %s: in %a;@," x c_ty t) envi';
  pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt ";@,")
        (fun fmt (x,t) -> fprintf fmt "signal %s: out %a" x c_ty t) fmt envo';
  fprintf fmt ");@]@]@,";
  fprintf fmt "end component;@,@,";

  List.iter (fun (x,t) -> fprintf fmt "signal %s: %a;@," x c_ty t) envi';
  List.iter (fun (x,t) -> fprintf fmt "signal %s: %a;@," x c_ty t) envo';

  fprintf fmt "signal start: std_logic;@]@,";
  fprintf fmt "signal rdy: std_logic;@]@,";
  fprintf fmt "type write_state_t is (Idle, StartAsserted);@,";
  fprintf fmt "signal write_state: write_state_t;@]@,";

  if !allow_heap_access || !allow_heap_assign then
    fprintf fmt "signal caml_heap_base : std_logic_vector(31 downto 0);@,";

  fprintf fmt "@[<v 2>begin@,";
  fprintf fmt "@[<v 2>%s_CC : component %s@," name name;
  fprintf fmt "port map (@[< v>";
  fprintf fmt "clk => clock_clk,@,";
  fprintf fmt "reset => reset_reset,@,";
  fprintf fmt "start => start,@,";
  fprintf fmt "rdy => rdy,@,";
(*
  fprintf fmt "avm_rm_address => avm_rm_address,@,";
  fprintf fmt "avm_rm_read => avm_rm_read,@,";
  fprintf fmt "avm_rm_readdata => avm_rm_readdata,@,";
  fprintf fmt "avm_rm_waitrequest => avm_rm_waitrequest,@,";
  *)
  List.iter (fun (x,_) -> fprintf fmt "%s => %s,@," x x) envi;
    pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt ",@,")
        (fun fmt (x,_) -> fprintf fmt "%s => %s" x x) fmt envo;
  fprintf fmt "@]);@]@,@,";
  fprintf fmt "WRITE: process (clock_clk, reset_reset)@,";
  fprintf fmt "@[<v 2>begin@,";
  fprintf fmt "@[<v 2>if reset_reset = '1' then@,";
  fprintf fmt "write_state <= Idle;@]@,";
  fprintf fmt "@[<v 2>elsif rising_edge(clock_clk) then@,";
  fprintf fmt "@[<v 2>case write_state is@,";
  fprintf fmt "@[<v 2>when StartAsserted =>@,";
  fprintf fmt "start <= '0';@,";
  fprintf fmt "write_state <= Idle;@]@,";
  fprintf fmt "@[<v 2>when Idle =>@,";
  fprintf fmt "@[<v 2>if avs_s0_write = '1' then@,";
  fprintf fmt "@[<v 2>case avs_s0_address is@,";
  fprintf fmt "@[<v 2>when \"0000\" => -- writing CSR asserts start  for one clock period@,";
  fprintf fmt "start <= '1';@,";
  fprintf fmt "write_state <= StartAsserted;@]@,";

  List.iteri (fun i (x,ty) ->
   fprintf fmt "@[<v 2>when \"%s\" => %s <= %a;@]@,"
       (bin_of_int (i+1)) x (conversion_from_vect ty) "avs_s0_writedata") envi';

  (if !allow_heap_access || !allow_heap_assign then
    fprintf fmt "@[<v 2>when \"%s\" => caml_heap_base <= avs_s0_writedata;@]@,"
             (bin_of_int (len+1)));

  fprintf fmt "when others => NULL;@]@,";
  fprintf fmt "end case;@]@,";
  fprintf fmt "end if;@]@,";
  fprintf fmt "end case;@]@,";
  fprintf fmt "end if;@]@,";
  fprintf fmt "end process;@]@,";
  fprintf fmt "READ: process (clock_clk)@,";
  fprintf fmt "@[<v 2>begin@,";
  fprintf fmt "@[<v 2>if rising_edge(clock_clk) then@,";
  fprintf fmt "@[<v 2>if avs_s0_read = '1' then@,";
  fprintf fmt "@[<v 2>case avs_s0_address is@,";
  fprintf fmt "when \"%s\" => @[<v>avs_s0_readdata <= X\"0000000\" & \"000\" & rdy;@," (bin_of_int 0);
  fprintf fmt "-- when reading CSR, bit 0 is rdy@]@,";

  List.iteri (fun i (x,ty) ->
   fprintf fmt "@[<v 2>when \"%s\" => %a;@]@,"
       (bin_of_int (i+1)) (set_result "avs_s0_readdata" ty) x) (envi'@envo');

  fprintf fmt "when others => null;@,";
  fprintf fmt "end case;@]@,";
  fprintf fmt "end if;@]@,";
  fprintf fmt "end if;@]@,";
  fprintf fmt "end process;@]@,";
  fprintf fmt "end architecture;@]@,"

let t_val ty fmt x =
 let open Typ in
  match ty with
  | TConst TStd_logic
  | TConst TBool ->
      fprintf fmt "Bool_val(%s)" x
  | TConst TInt ->
      fprintf fmt "Int_val(%s)" x
  | TConst TUnit ->
      fprintf fmt "Bool_val(%s)" x (* Bool_val is used to construct unit value *)
  | (TPtr _ | TVar _) ->
      pp_print_text fmt x
  | TPacket _ ->
      assert false

let val_t ty fmt cb =
  let open Typ in
  fprintf fmt "return ";
  match ty with
  | TConst  TInt | TConst  TBool -> (* immediate *)
      pp_print_text fmt "Val_int(";
      cb fmt;
      pp_print_text fmt ")"
  | _ -> (* already a value *)
      cb fmt

let mk_platform_bindings fmt (envi,envo,_) name =
  let envi = List.filter (fun (x,_) -> x <> "start") envi in
  (* ne gère pas les sorties multiples *)
  fprintf fmt "@[<v>@[<v 2>uint32_t caml_nios_%s_cc(" name;
  fprintf fmt "@[<hov>";
  pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt ",@,")
        (fun fmt (x,_) -> fprintf fmt "uint32_t %s" x) fmt envi;
  fprintf fmt "@]) {@,";
  val_t (List.assoc "result" envo) fmt (fun fmt ->
    fprintf fmt "nios_%s_cc(" name;
    fprintf fmt "@[<hov>";
    pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt ",@,")
        (fun fmt (x,ty) -> t_val ty fmt x) fmt envi;
    fprintf fmt "@]");
  fprintf fmt ");";
  fprintf fmt "@]@,}@,@]"


let t_C ty =
 let open Typ in
  match ty with
  | TConst _ -> "int"
  | (TPtr _ | TVar _) -> "uint32_t"
  | TPacket _ ->
      assert false

let up = String.uppercase_ascii
let low = String.lowercase_ascii

let mk_platform_c fmt (envi,envo,_) name =
  let envi = List.filter (fun (x,_) -> x <> "start") envi in
  let envo = List.filter (fun (x,_) -> x <> "rdy") envo in
  

  if !allow_heap_alloc then 
    fprintf fmt "#include \"../../../../../omicrob/src/byterun/vm/gc.h\"@,@,";

  (* rdy : Control/status register for the custom component *)
  let upName = up name in
  fprintf fmt "@[<v>";
  fprintf fmt "#define %s_CC_CTL 0@," upName;

  let regs = envi@envo in
  List.iteri (fun i (x,_) ->
    fprintf fmt "#define %s_CC_%s %d@," upName (up x) (i+1)) regs;

  if !allow_heap_access || !allow_heap_assign then
    fprintf fmt "#define %s_CC_CAML_HEAP_BASE %d@," upName (List.length regs + 1);

  fprintf fmt "@,@[<v 2>int nios_%s_cc(" (low name);
  fprintf fmt "@[<hov>";
  pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt ",@,")
        (fun fmt (x,t) -> fprintf fmt "%s %s" (t_C t) (low x)) fmt envi;
  fprintf fmt "@]){@,";
  fprintf fmt "alt_u32 result;@,";

  if !flag_print_compute_time then
     fprintf fmt "int __dt = nios_timer_get_us();@,@,";

  (* fprintf fmt "@[<v 2>while (!IORD(%s_CC_BASE, %s_CC_CTL))@," upName upName;
  fprintf fmt "; // Get RDY status by reading control/status reg@]@,";  *)
  fprintf fmt "// Write arguments@,";
  List.iter (fun (x,_) ->
    fprintf fmt "IOWR(%s_CC_BASE, %s_CC_%s, %s);@," upName upName (up x) (low x)) envi;

  if !allow_heap_access || !allow_heap_assign then
    fprintf fmt "IOWR(%s_CC_BASE, %s_CC_CAML_HEAP_BASE, ocaml_ram_heap);@," upName upName;

  fprintf fmt "@,IOWR(%s_CC_BASE, %s_CC_CTL, 1);" upName upName;

  if !allow_heap_alloc then
    fprintf fmt "@,IOWR(%s_CC_BASE, %s_CC_ALLOC_RDY, 1);" upName upName;

  fprintf fmt "@,@,@[<v 2>while ( IORD(%s_CC_BASE, %s_CC_CTL) == 0 ){ // Wait for rdy@," upName upName;
  
  if !allow_heap_alloc then begin
     fprintf fmt "@[<v 2>if (IORD(%s_CC_BASE, %s_CC_ALLOC_RQ)){@," upName upName;
     fprintf fmt "value v;@,";
     fprintf fmt "@,IOWR(%s_CC_BASE, %s_CC_ALLOC_RDY, 0);" upName upName;
     fprintf fmt "@,OCamlAlloc(v, IORD(%s_CC_BASE, %s_CC_ALLOC_SIZE), IORD(%s_CC_BASE, %s_CC_ALLOC_TAG));@," upName upName upName upName;
     fprintf fmt "@,IOWR(%s_CC_BASE, %s_CC_ALLOC_RDY, 1);" upName upName;
     fprintf fmt "@,IOWR(%s_CC_BASE, %s_CC_ALLOC_PTR, v);" upName upName;
     fprintf fmt "@]@,}"
  end;
  fprintf fmt "@]@,}@,";
  fprintf fmt "result = IORD(%s_CC_BASE, %s_CC_RESULT); // Read result@," upName upName;

  if !allow_trap then begin
    fprintf fmt "int trap = IORD(%s_CC_BASE, %s_CC_TRAP); // Read trap@," upName upName;
    fprintf fmt "if (trap > 0){ caml_raise_failure(\"%s\"); }@," upName;
  end;

  (* ********************************************************************** *)
  if !flag_print_compute_time || !flag_print_compute_time_short then begin
     fprintf fmt "@,__dt = nios_timer_get_us() - __dt;@,";
     if !flag_print_compute_time_short then
          fprintf fmt "@,printf(\"%%d,\n\",__dt);"
     else fprintf fmt "@,printf(\"\\nellapsed time : %%d us\\n\",__dt);@,@,";
  end;
  (* ********************************************************************** *)
  fprintf fmt "return result;@]";
  fprintf fmt "@,@]}@,@]@."

let mk_platform_h fmt (envi,envo,_) name =
  let envi = List.filter (fun (x,_) -> x <> "start") envi in
  fprintf fmt "@[<hov>int nios_%s_cc(" (low name);
  fprintf fmt "@[<hov>";
  pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt ",@,")
        (fun fmt (x,t) -> fprintf fmt "%s %s" (t_C t) (low x)) fmt envi;
  fprintf fmt "@]);@]@,"

let mk_simul_c fmt (envi,envo,_) name =
  let envi = List.filter (fun (x,_) -> x <> "start") envi in
  fprintf fmt "@[<v>@[<v 2>int nios_%s_cc(" (low name);
 fprintf fmt "@[<hov>";
  pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt ",@,")
        (fun fmt (x,t) -> fprintf fmt "%s %s" (t_C t) (low x)) fmt envi;
  fprintf fmt "@]){@,";
   (* fprintf fmt "@[<hov>printf(\"nios_gcd_cc(";
  pp_print_list
    ~pp_sep:(fun fmt () -> fprintf fmt ",@,")
    (fun fmt _ -> fprintf fmt "%s" "%d") fmt envi;

  if !allow_heap_access || !allow_heap_assign then
    fprintf fmt ", caml_heap_base";      (* todo : cf. O2B list2 : printf("nios_list_reduce_cc(%x)\n", (unsigned int)v); *)

  pp_print_text fmt ")\\n\"";
  List.iter (fun (x,t) -> fprintf fmt ", %s" (low x)) envi;
  fprintf fmt ");@]@,";*)
  fprintf fmt "return 1;@]";
  fprintf fmt "@,@]}@,@]@."

let mk_simul_h = mk_platform_h


let rec t_ML fmt ty =
  let open Typ in
  match ty with
  | TConst TStd_logic
  | TConst TBool ->
      pp_print_text fmt "bool"
  | TConst TInt ->
      pp_print_text fmt "int"
  | TConst TUnit ->
      pp_print_text fmt "unit"
  | TPtr (x,[]) ->
      pp_print_text fmt x
  | TPtr (x,tys) ->
      fprintf fmt "(%a) %s"
      (fun fmt -> pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt ",")
        t_ML fmt) tys x
  | TVar n ->
      fprintf fmt "'a%d" n
  | TPacket _ ->
      assert false

(* Seule la variable de sortie [result] est ramenée vers OCaml.
   Les entrées du circuits appraissent dans la signature ml
   sous-forme d'arguments étiquettés (labels).
   S'il n'y a aucune entrée, l'externals est de type [unit -> t],
   i.e. on rajoute un argument unit pour obtenir un type effectivement fonctionnel. *)

let mk_platform_ml ?(labels=true) fmt (envi,envo,_) name =
  let envi = List.filter (fun (x,_) -> x <> "start") envi in

  let envi = List.filter (fun (x,_) -> match x with "alloc_ptr" | "alloc_rdy" -> false | _ -> true) envi in

  let envo = List.filter (fun (x,_) -> match x with "alloc_rq" | "alloc_size" | "alloc_tag" -> false | _ -> true) envo in


  let tres = List.assoc "result" envo in

  fprintf fmt "@[<hov>external %s : " (low name);
  List.iter (fun (x,t) ->
                if labels then (fprintf fmt "%s:" (low x));
                fprintf fmt "%a -> " t_ML t) envi;
  if envi = [] then fprintf fmt "unit -> ";
  fprintf fmt "%a = \"caml_nios_%s_cc\"" t_ML tres  (low name);
  
  if not !allow_heap_alloc then
    pp_print_text fmt " [@@noalloc]";
  
  fprintf fmt "@]@."

let mk_platform_mli = mk_platform_ml


let mk_vhdl_with_cc ?labels ?xs_opt circuit =
  let open Esml in
  let {x=name;vars} = circuit in
  let envi = List.filter_map (function (In,x,t) -> Some (x,t) | _ -> None) vars in
  let envo = List.filter_map (function (Out,x,t) ->  Some (x,t) | _ -> None) vars in
  let envl = List.filter_map (function (Local,x,t) ->  Some (x,t) | _ -> None) vars in
  let vars = envi,envo,envl in

  let open Filename in
  let (^^) = Filename.concat in
  let open Format in

  let name = String.lowercase_ascii name in

  let dst = "gen"
  and rtl_dir = "rtl"
  and qsys_dir = "qsys"
  and c_dir   = "c"
  and ml_dir  = "ml" in
  let desc_name         = dst ^^ rtl_dir ^^  (name ^".vhdl")
  and cc_name           = dst ^^ rtl_dir ^^  (name ^"_cc.vhdl")
  and misc_name         = dst ^^ rtl_dir ^^ "misc" ^^ (name ^ "_misc.vhdl")
  and bindings_name     = dst ^^ c_dir   ^^  (name ^ "_platform-bindings.c")
  and platform_c_name   = dst ^^ c_dir   ^^  (name ^ "_platform.c")
  and platform_h_name   = dst ^^ c_dir   ^^  (name ^ "_platform.h")
  and simul_c_name      = dst ^^ c_dir   ^^  (name ^ "_simul.c")
  and simul_h_name      = dst ^^ c_dir   ^^  (name ^ "_simul.h")
  and prelude_name      = dst ^^ ml_dir  ^^  "prelude.ml"
  and platform_ml_name  = dst ^^ ml_dir  ^^  (name ^ "_platform.ml")
  and platform_mli_name = dst ^^ ml_dir  ^^  (name ^ "_platform.mli")
  and hw_tcl_name       = dst ^^ qsys_dir ^^  (name ^ "_cc_hw.tcl")
  and ext_tcl_name      = dst ^^ qsys_dir ^^  (name ^ "_cc_ext.tcl")
  in
  let fmt = std_formatter in
  let desc_oc = open_out desc_name
  and cc_oc = open_out cc_name
  and misc_cc = open_out misc_name
  and bindings_oc = open_out bindings_name
  and platform_c_oc = open_out platform_c_name
  and platform_h_oc = open_out platform_h_name
  and simul_c_oc = open_out simul_c_name
  and simul_h_oc = open_out simul_h_name
  and prelude_oc = open_out prelude_name
  and platform_ml_oc = open_out platform_ml_name
  and platform_mli_oc = open_out platform_mli_name
  and hw_tcl_oc = open_out hw_tcl_name
  and ext_tcl_oc = open_out ext_tcl_name
  in
  set_formatter_out_channel desc_oc;
  Esml2vhdl.compile_esml_circuit fmt circuit;

  set_formatter_out_channel misc_cc;
  mk_package name fmt;
  pp_print_flush fmt ();

  let vars' =
    let (envi,envo,l) = vars in
    let f = List.filter (fun (x,t) ->
      match x with
      | "start"
      | "rdy"
      | "avm_rm_read"
      | "avm_rm_waitrequest"
      | "avm_rm_readdata"
      | "avm_rm_address"
      | "avm_wm_writedata"
      | "avm_wm_address"
      | "avm_wm_write"
      | "avm_wm_waitrequest"
      | "caml_heap_base"
        -> false
      | _ -> true) in
    let envi = f envi in
    let envo = f envo in
    let envi = match xs_opt with
               | None -> envi
               | Some xs -> List.map (fun x -> (x,List.assoc x envi)) xs in
    (envi,envo,l)
  in

  set_formatter_out_channel prelude_oc;
  Ast.pprint_code_typ_constr_decl fmt;
  pp_print_flush fmt ();

  set_formatter_out_channel cc_oc;
  gen_cc fmt vars vars' name;
  pp_print_flush fmt ();

  set_formatter_out_channel bindings_oc;
  mk_platform_bindings fmt vars' name;
  pp_print_flush fmt ();

  set_formatter_out_channel platform_c_oc;
  mk_platform_c fmt vars' name;
  pp_print_flush fmt ();

  set_formatter_out_channel platform_h_oc;
  mk_platform_h fmt vars' name;
  pp_print_flush fmt ();

  set_formatter_out_channel simul_c_oc;
  mk_simul_c fmt vars' name;
  pp_print_flush fmt ();

  set_formatter_out_channel simul_h_oc;
  mk_simul_h fmt vars' name;
  pp_print_flush fmt ();

  set_formatter_out_channel platform_ml_oc;
  mk_platform_ml ?labels fmt vars' name;
  pp_print_flush fmt ();

  set_formatter_out_channel platform_mli_oc;
  mk_platform_mli ?labels fmt vars' name;
  pp_print_flush fmt ();

  set_formatter_out_channel hw_tcl_oc;
  Gen_hw_tcl.mk_hw_tcl name fmt;
  pp_print_flush fmt ();

  set_formatter_out_channel ext_tcl_oc;
  Gen_hw_tcl.mk_ext_gen_qsys name fmt;
  pp_print_flush fmt ();

  close_out desc_oc;
  close_out cc_oc;
  close_out bindings_oc;
  close_out platform_c_oc;
  close_out platform_h_oc;
  close_out simul_c_oc;
  close_out simul_h_oc;
  close_out prelude_oc;
  close_out platform_ml_oc;
  close_out platform_mli_oc;
  close_out hw_tcl_oc;
  close_out ext_tcl_oc;

  Printf.printf "  info: circuit  \"%s\"  generated in folder gen/.\n" name
