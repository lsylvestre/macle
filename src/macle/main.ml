(* ******************************************************************* *)
(*                                                                     *)
(*                         Macle (ML accelerator)                      *)
(*             a tool for accelerating OCaml programs on FPGA          *)
(*                                                                     *)
(* ******************************************************************* *)

type lang = PLATFORM

let inputs = ref ([] : string list)

let add_file f = inputs := !inputs @ [f]

let flag_lang = ref PLATFORM

let flag_verbose = ref false

let flag_vhdl_only = ref false
let flag_show_ast = ref false
let flag_show_typed_ast = ref false
let flag_show_inlined_ast = ref false

let flag_propagation = ref true (* optimisation *)
let flag_let_floating = ref true
let flag_enforce_tail_rec = ref false

let flag_simulation_typed = ref false
let flag_simulation_inlined = ref false

let simulation_level = ref 0
let flag_vsml2esml = ref false

let mult_clock_factor = ref 1

let () =
 let set_lang c =
   Arg.Unit (fun () -> flag_lang := c)
 and set_int r = 
   Arg.Int (fun n -> r := n)
 in
 Arg.parse [
      ("-time",            Arg.Set Gen_platform.flag_print_compute_time,
                           "instrument the produced code to measure\
                           \ the compute time");
      ("-time-short",      Arg.Set Gen_platform.flag_print_compute_time_short,
                           "variant of the -time option");
      ("-show-ast",         Arg.Set flag_show_ast,
                            "display the syntax tree (AST)\
                            \ of the input program");
      ("-show-typed-ast",   Arg.Set flag_show_typed_ast,
                            "display the typed AST\
                            \ of the input program");
      ("-show-inlined-ast", Arg.Set flag_show_inlined_ast,
                            "display the typed AST\
                            \ of the input program\
                            \ after inlining");
      ("-no-propagation",   Arg.Clear flag_propagation,
                            "disable atom propagation");
      ("-no-let-floating",   Arg.Clear flag_let_floating,
                            "disable let floating");
      ("-enforce-tail-rec",   Arg.Set flag_enforce_tail_rec,
                             "enforce recursive functions to tail recursive");
      ("-verb",             Arg.Set flag_verbose,
                            "print additionnal informations");
      ("-vsml2esml",        Arg.Set flag_vsml2esml,
                            "alternative compilation scheme\
                            \ (from VSML to ESML)");
      ("-simul",            Arg.Set flag_simulation_typed,
                            "generate an OCaml program from typed AST");
      ("-ocaml-backend",    Arg.Set flag_simulation_inlined,
                            "generate an OCaml program from inlined AST");

      ("-app",              set_lang PLATFORM,
                            "Macle input (default)");
      ("-vhdl",             Arg.Set flag_vhdl_only,
                            "generates a VHDL source without other\
                            \ files needed to extend an O2B platform");
      ("-nios2-freq-multiplier", 
                            set_int mult_clock_factor,
                            "multiply the frequency of the Nios II softcore\
                            \ processor by the given factor (expected value is\
                            \ 1, 2 or 3)") ]

      add_file "Usage:\n  ./compile files"

let mk_vhdl ?labels ?(with_cc=false) c =
  if !flag_vhdl_only then
    Esml2vhdl.compile_esml_circuit Format.std_formatter c
  else Gen_platform.mk_vhdl_with_cc ?labels c

let parse filename =
  let ic = open_in filename in

  let name = Filename.(remove_extension @@ basename filename) in

  Esml2vhdl.allow_heap_access := false;
  Esml2vhdl.allow_heap_assign := false;

  if !flag_verbose then Printf.printf "\ninfos: process %s:\n" filename;

  try
    let lexbuf = Lexing.from_channel ic in
    let (circuits,main) = Parser.platform_macle Lexer.token lexbuf in

    if !flag_simulation_typed || !flag_simulation_inlined then
      Ast.pprint_code_typ_constr_decl Format.std_formatter;

    List.iter
     (function c ->
          (* initialize heap_access / heap_assign to none *)
          Esml2vhdl.allow_heap_access := false;
          Esml2vhdl.allow_heap_assign := false;
          Esml2vhdl.allow_trap := false;
          Esml2vhdl.allow_stack := false;
          if !flag_show_ast then
            Pprint_ast.PP_MACLE.pp_circuit Format.err_formatter c;

          if !flag_enforce_tail_rec then
            Check_tailrec.check_tailrec c;

          let c = Typing.typing_circuit c in

          let c = Ast_rename.rename_ast c in

          if !flag_show_typed_ast then
            Pprint_ast.PP_TMACLE.pp_circuit Format.err_formatter c;

          if !flag_simulation_typed
          then Ast2ocaml.pp_circuit Format.std_formatter c else

          let c = Safe_array_access.safe_array_access_circuit c in

          let c = Macro_expansion.expand_circuit c in

          let c = Inline.inline_circuit c in

          let c = Anf.anf_circuit c in

          let c = Ast_rename.rename_ast c in

          let c = Equations.rewrite_map_circuit c in

          let c = if !flag_propagation
                  then Propagation.atom_propagation c
                  else c
          in

          let c = if !flag_let_floating then
                  Let_floating.circuit_let_floating c else c in

          let c = Derecursivation.derecursivation_circuit c in
          let c = Anf.anf_circuit c in
          if !flag_show_inlined_ast then
            Pprint_ast.PP_TMACLE.pp_circuit Format.err_formatter c;

          let c = if !flag_propagation
                  then Propagation.atom_propagation c
                  else c
          in

          let c = if !flag_let_floating then
                  Let_floating.circuit_let_floating c else c in

          if !flag_simulation_inlined
          then Ast2ocaml.pp_circuit Format.std_formatter c else
          let c = Ast_rename.rename_ast c in

          let c = Vsml2esml.vsml2esml c;
          in
          mk_vhdl ~labels:false c
        ) circuits;

        let open Format in
        if !flag_simulation_typed || !flag_simulation_inlined then (
          fprintf std_formatter "@,%s" main;
          fprintf std_formatter ";;@]@."
        ) else (
        let dst = "gen" in
        let app_name  = Filename.(concat dst (concat "apps" (name ^".ml"))) in
        let fmt = std_formatter in
        let app_oc = open_out app_name in
        set_formatter_out_channel app_oc;

        (* code added at the top of the file main.ml
           of each application generated
         *)
        fprintf fmt "@[<v>open Platform ;;@,@,";
        fprintf fmt "let print_int = Serial.write_int ;;@,";
        fprintf fmt "let print_string = Serial.write_string ;;@,@,";

        if !Gen_platform.flag_print_compute_time ||
           !Gen_platform.flag_print_compute_time_short then
          fprintf fmt "Timer.init() ;;@,@,";

        if !Gen_platform.flag_print_compute_time then
          (fprintf fmt "let chrono f =@,";
           fprintf fmt "  let t1 = Timer.get_us () in@,";
           fprintf fmt "  let _ = f () in@,";
           fprintf fmt "  let t2 = Timer.get_us () in@,";
           fprintf fmt "  t2-t1 ;;@,@,");
        
        fprintf fmt "@[<b>";
        pp_print_text fmt main;
        fprintf fmt "@]@]@.";
        pp_print_flush fmt ();
        close_out app_oc;
        
        let qsys_script_oc = open_out "gen/qsys/platform.tcl" in
        set_formatter_out_channel qsys_script_oc;
        Gen_platform_tcl.gen_platform_tcl ~mult_clock_factor:!mult_clock_factor fmt;
        pp_print_flush fmt ();
        close_out qsys_script_oc
      );
     close_in ic
  with e ->
    close_in ic; raise e ;;


let () =
  List.iter parse !inputs
