open Format   
   
let mk_hw_tcl name fmt size_for_coding_arguments_id =
  let name_cc = name ^ "_cc" in
  let name_cc_up = String.uppercase_ascii name_cc in
  fprintf fmt "@[<v># TCL File Generated __for__ Component Editor 15.1@,@," ;
  fprintf fmt "# %s \"%s\"@,@," name_cc name_cc                             ;
  fprintf fmt "# request TCL package from ACDS 15.1@,"                      ;
  fprintf fmt "#@,"                                                         ;
  fprintf fmt "package require -exact qsys 15.1@,@,"                        ;
  fprintf fmt "#@,"                                                         ;
  fprintf fmt "#module %s@," name_cc_up                                     ;
  fprintf fmt "#@,"                                                         ;
  fprintf fmt "set_module_property DESCRIPTION \"\"@,"                      ;
  fprintf fmt "set_module_property NAME %s@," name_cc_up                    ;
  fprintf fmt "set_module_property VERSION 1.0@,"                           ;
  fprintf fmt "set_module_property INTERNAL false@,"                        ;
  fprintf fmt "set_module_property OPAQUE_ADDRESS_MAP true@,"               ;
  fprintf fmt "set_module_property GROUP my_ips@,"                          ;
  fprintf fmt "set_module_property AUTHOR \"\"@,"                           ;
  fprintf fmt "set_module_property DISPLAY_NAME %s@," name_cc_up            ;
  fprintf fmt "set_module_property INSTANTIATE_IN_SYSTEM_MODULE true@,"     ;
  fprintf fmt "set_module_property EDITABLE true@,"                         ;
  fprintf fmt "set_module_property REPORT_TO_TALKBACK false@,"              ;
  fprintf fmt "set_module_property ALLOW_GREYBOX_GENERATION false@,"        ;
  fprintf fmt "set_module_property REPORT_HIERARCHY false@,@,"              ;
  fprintf fmt "#@,"                                                         ;
  fprintf fmt "# file sets@,"                                               ;
  fprintf fmt "#@,"                                                         ;
  fprintf fmt "add_fileset QUARTUS_SYNTH QUARTUS_SYNTH \"\" \"\"@,"         ;
  fprintf fmt "set_fileset_property QUARTUS_SYNTH TOP_LEVEL avs_%s@," name  ;
  fprintf fmt "set_fileset_property QUARTUS_SYNTH ENABLE_RELATIVE_INCLUDE_PATHS false@,"   ;
  fprintf fmt "set_fileset_property QUARTUS_SYNTH ENABLE_FILE_OVERWRITE_MODE false@,"      ;
  fprintf fmt "add_fileset_file %s.vhd VHDL PATH ../rtl/%s.vhd TOP_LEVEL_FILE@," name_cc name_cc ;
  fprintf fmt "add_fileset_file %s.vhd VHDL PATH ../rtl/%s.vhd@," name name                ;
  fprintf fmt "add_fileset_file %s_misc.vhd VHDL PATH ../rtl/misc/%s_misc.vhd@," name name ;
  fprintf fmt "@,#@,"                                   ;
  fprintf fmt "@,# parameters@,"                        ;
  fprintf fmt "#@,@,@,"                                 ;
  fprintf fmt "@,#@,"                                   ;
  fprintf fmt "@,# display items@,"                     ;
  fprintf fmt "#@,@,@,"                                 ;

  fprintf fmt "@,#@,"                                   ;
  fprintf fmt "@,# connection point s0@,"               ;
  fprintf fmt "#@,@,"                                   ;
  fprintf fmt "add_interface s0 avalon end@,"           ;
  fprintf fmt "set_interface_property s0 addressUnits WORDS@,"                ;
  fprintf fmt "set_interface_property s0 associatedClock clock@,"             ;
  fprintf fmt "set_interface_property s0 associatedReset reset@,"             ;
  fprintf fmt "set_interface_property s0 bitsPerSymbol 8@,"                   ;
  fprintf fmt "set_interface_property s0 burstOnBurstBoundariesOnly false@,"  ;
  fprintf fmt "set_interface_property s0 burstcountUnits WORDS@,"             ;
  fprintf fmt "set_interface_property s0 explicitAddressSpan 0@,"             ;
  fprintf fmt "set_interface_property s0 holdTime 0@,"                        ;
  fprintf fmt "set_interface_property s0 linewrapBursts false@,"              ;
  fprintf fmt "set_interface_property s0 maximumPendingReadTransactions 0@,"  ;
  fprintf fmt "set_interface_property s0 maximumPendingWriteTransactions 0@," ;
  fprintf fmt "set_interface_property s0 readLatency 0@,"                     ;
  fprintf fmt "set_interface_property s0 readWaitTime 1@,"                    ;
  fprintf fmt "set_interface_property s0 setupTime 0@,"                       ;
  fprintf fmt "set_interface_property s0 timingUnits Cycles@,"                ;
  fprintf fmt "set_interface_property s0 writeWaitTime 0@,"                   ;
  fprintf fmt "set_interface_property s0 ENABLED true@,"                      ;
  fprintf fmt "set_interface_property s0 EXPORT_OF \"\"@,"                    ;
  fprintf fmt "set_interface_property s0 PORT_NAME_MAP \"\"@,"                ;
  fprintf fmt "set_interface_property s0 CMSIS_SVD_VARIABLES \"\"@,"          ;
  fprintf fmt "set_interface_property s0 SVD_ADDRESS_GROUP \"\"@,"            ;
  fprintf fmt "@,"                                                            ;
  fprintf fmt "add_interface_port s0 avs_s0_address address Input %d@,"
    size_for_coding_arguments_id ;
  fprintf fmt "add_interface_port s0 avs_s0_write write Input 1@,"            ;
  fprintf fmt "add_interface_port s0 avs_s0_writedata writedata Input 32@,"   ;
  fprintf fmt "add_interface_port s0 avs_s0_read read Input 1@,"              ;
  fprintf fmt "add_interface_port s0 avs_s0_readdata readdata Output 32@,"    ;
  fprintf fmt "set_interface_assignment s0 embeddedsw.configuration.isFlash 0@,"              ;
  fprintf fmt "set_interface_assignment s0 embeddedsw.configuration.isMemoryDevice 0@,"       ;
  fprintf fmt "set_interface_assignment s0 embeddedsw.configuration.isNonVolatileStorage 0@," ;
  fprintf fmt "set_interface_assignment s0 embeddedsw.configuration.isPrintableDevice 0@,"    ;
  fprintf fmt "@,@,"  ;

  if !Esml2vhdl.allow_heap_access then begin
    fprintf fmt "#@,"                                                            ;
    fprintf fmt "@,# connection point rm@,"                                      ;
    fprintf fmt "#@,@,"                                                          ;
    fprintf fmt "add_interface rm avalon start@,"                                ;
    fprintf fmt "set_interface_property rm addressUnits SYMBOLS@,"               ;
    fprintf fmt "set_interface_property rm associatedClock clock@,"              ;
    fprintf fmt "set_interface_property rm associatedReset reset@,"              ;
    fprintf fmt "set_interface_property rm bitsPerSymbol 8@,"                    ;
    fprintf fmt "set_interface_property rm burstOnBurstBoundariesOnly false@,"   ;
    fprintf fmt "set_interface_property rm burstcountUnits WORDS@,"              ;
    fprintf fmt "set_interface_property rm doStreamReads false@,"                ;
    fprintf fmt "set_interface_property rm doStreamWrites false@,"               ;
    fprintf fmt "set_interface_property rm holdTime 0@,"                         ;
    fprintf fmt "set_interface_property rm linewrapBursts false@,"               ;
    fprintf fmt "set_interface_property rm maximumPendingReadTransactions 0@,"   ;
    fprintf fmt "set_interface_property rm maximumPendingWriteTransactions 0@,"  ;
    fprintf fmt "set_interface_property rm readLatency 0@,"                      ;
    fprintf fmt "set_interface_property rm readWaitTime 1@,"                     ;
    fprintf fmt "set_interface_property rm setupTime 0@,"                        ;
    fprintf fmt "set_interface_property rm timingUnits Cycles@,"                 ;
    fprintf fmt "set_interface_property rm writeWaitTime 0@,"                    ;
    fprintf fmt "set_interface_property rm ENABLED true@,"                       ;
    fprintf fmt "set_interface_property rm EXPORT_OF \"\"@,"                     ;
    fprintf fmt "set_interface_property rm PORT_NAME_MAP \"\"@,"                 ;
    fprintf fmt "set_interface_property rm CMSIS_SVD_VARIABLES \"\"@,"           ;
    fprintf fmt "set_interface_property rm SVD_ADDRESS_GROUP \"\"@,"             ;
    fprintf fmt "@,"                                                             ;
    fprintf fmt "add_interface_port rm avm_rm_address address Output 32@,"       ;
    fprintf fmt "add_interface_port rm avm_rm_read read Output 1@,"              ;
    fprintf fmt "add_interface_port rm avm_rm_readdata readdata Input 32@,"      ;
    fprintf fmt "add_interface_port rm avm_rm_waitrequest waitrequest Input 1@," ;
    fprintf fmt "@,@,"                                                           ;
  end;


  if !Esml2vhdl.allow_heap_assign then begin
    fprintf fmt "#@,"                                                            ;
    fprintf fmt "@,# connection point wm@,"                                      ;
    fprintf fmt "#@,@,"                                                          ;
    fprintf fmt "add_interface wm avalon start@,"                                ;
    fprintf fmt "set_interface_property wm addressUnits SYMBOLS@,"               ;
    fprintf fmt "set_interface_property wm associatedClock clock@,"              ;
    fprintf fmt "set_interface_property wm associatedReset reset@,"              ;
    fprintf fmt "set_interface_property wm bitsPerSymbol 8@,"                    ;
    fprintf fmt "set_interface_property wm burstOnBurstBoundariesOnly false@,"   ;
    fprintf fmt "set_interface_property wm burstcountUnits WORDS@,"              ;
    fprintf fmt "set_interface_property wm doStreamReads false@,"                ;
    fprintf fmt "set_interface_property wm doStreamWrites false@,"               ;
    fprintf fmt "set_interface_property wm holdTime 0@,"                         ;
    fprintf fmt "set_interface_property wm linewrapBursts false@,"               ;
    fprintf fmt "set_interface_property wm maximumPendingReadTransactions 0@,"   ;
    fprintf fmt "set_interface_property wm maximumPendingWriteTransactions 0@,"  ;
    fprintf fmt "set_interface_property wm readLatency 0@,"                      ;
    fprintf fmt "set_interface_property wm readWaitTime 1@,"                     ;
    fprintf fmt "set_interface_property wm setupTime 0@,"                        ;
    fprintf fmt "set_interface_property wm timingUnits Cycles@,"                 ;
    fprintf fmt "set_interface_property wm writeWaitTime 0@,"                    ;
    fprintf fmt "set_interface_property wm ENABLED true@,"                       ;
    fprintf fmt "set_interface_property wm EXPORT_OF \"\"@,"                     ;
    fprintf fmt "set_interface_property wm PORT_NAME_MAP \"\"@,"                 ;
    fprintf fmt "set_interface_property wm CMSIS_SVD_VARIABLES \"\"@,"           ;
    fprintf fmt "set_interface_property wm SVD_ADDRESS_GROUP \"\"@,"             ;
    fprintf fmt "@,"                                                             ;
    fprintf fmt "add_interface_port wm avm_wm_address address Output 32@,"       ;
    fprintf fmt "add_interface_port wm avm_wm_write write Output 1@,"            ;
    fprintf fmt "add_interface_port wm avm_wm_writedata writedata Output 32@,"   ;
    fprintf fmt "add_interface_port wm avm_wm_waitrequest waitrequest Input 1@," ;
    fprintf fmt "@,@,"                                                           ;
  end;

  fprintf fmt "#@,"                                                       ;
  fprintf fmt "@,# connection point clock@,"                              ;
  fprintf fmt "#@,"                                                       ;
  fprintf fmt "add_interface clock clock end@,"                           ;
  fprintf fmt "set_interface_property clock clockRate 0@,"                ;
  fprintf fmt "set_interface_property clock ENABLED true@,"               ;
  fprintf fmt "set_interface_property clock EXPORT_OF \"\"@,"             ;
  fprintf fmt "set_interface_property clock PORT_NAME_MAP \"\"@,"         ;
  fprintf fmt "set_interface_property clock CMSIS_SVD_VARIABLES \"\"@,"   ;
  fprintf fmt "set_interface_property clock SVD_ADDRESS_GROUP \"\"@,"     ;
  fprintf fmt "@,"                                                        ;
  fprintf fmt "add_interface_port clock clock_clk clk Input 1@,"          ;
  fprintf fmt "@,"                                                        ;

  fprintf fmt "#@,"                                                       ;
  fprintf fmt "@,# connection point reset@,"                              ;
  fprintf fmt "#@,"                                                       ;
  fprintf fmt "add_interface reset reset end@,"                           ;
  fprintf fmt "set_interface_property reset associatedClock clock@,"      ;
  fprintf fmt "set_interface_property reset synchronousEdges DEASSERT@,"  ;
  fprintf fmt "set_interface_property reset ENABLED true@,"               ;
  fprintf fmt "set_interface_property reset EXPORT_OF \"\"@,"             ;
  fprintf fmt "set_interface_property reset PORT_NAME_MAP \"\"@,"         ;
  fprintf fmt "set_interface_property reset CMSIS_SVD_VARIABLES \"\"@,"   ;
  fprintf fmt "set_interface_property reset SVD_ADDRESS_GROUP \"\"@,"     ;
  fprintf fmt "@,"                                                        ;
  fprintf fmt "add_interface_port reset reset_reset reset Input 1@,"      ;

  fprintf fmt "@]@."

let idx = ref 10000000

let mk_ext_gen_qsys name fmt =
  let name_cc = name ^ "_cc" in
  let name_cc_up = String.uppercase_ascii name_cc in

  fprintf fmt "@[<v>add_instance %s %s 1.0@,@," name_cc name_cc_up    ;
  fprintf fmt "add_connection cpu.data_master %s.s0 avalon@," name_cc;
  fprintf fmt "set_connection_parameter_value cpu.data_master/%s.s0 arbitrationPriority {1}@," name_cc;
  fprintf fmt "set_connection_parameter_value cpu.data_master/%s.s0 baseAddress {0x%08d}@," name_cc !idx;
  fprintf fmt "set_connection_parameter_value cpu.data_master/%s.s0 defaultConnection {0}@," name_cc;

  idx := !idx + 1000;
(*
  if !Esml2vhdl.allow_heap_access then begin
    fprintf fmt "add_connection %s.rm onchip_memory.s1 avalon@," name_cc;
    fprintf fmt "set_connection_parameter_value %s.rm/onchip_memory.s1 arbitrationPriority {1}@," name_cc;
    fprintf fmt "set_connection_parameter_value %s.rm/onchip_memory.s1 baseAddress {0x00020000}@," name_cc;
    fprintf fmt "set_connection_parameter_value %s.rm/onchip_memory.s1 defaultConnection {0}@," name_cc;
  end;

  if !Esml2vhdl.allow_heap_access then begin
    fprintf fmt "add_connection %s.wm onchip_memory.s1 avalon@," name_cc;
    fprintf fmt "set_connection_parameter_value %s.wm/onchip_memory.s1 arbitrationPriority {1}@," name_cc;
    fprintf fmt "set_connection_parameter_value %s.wm/onchip_memory.s1 baseAddress {0x00020000}@," name_cc;
    fprintf fmt "set_connection_parameter_value %s.wm/onchip_memory.s1 defaultConnection {0}@," name_cc;
  end;
 *)
  let mem = "sdram" (* onchip_memory *) in
  if !Esml2vhdl.allow_heap_access then begin
    fprintf fmt "add_connection %s.rm %s.s1 avalon@," name_cc mem;
    fprintf fmt "set_connection_parameter_value %s.rm/%s.s1 arbitrationPriority {1}@," name_cc mem;
    fprintf fmt "set_connection_parameter_value %s.rm/%s.s1 baseAddress {0x00000000}@," name_cc mem;
    fprintf fmt "set_connection_parameter_value %s.rm/%s.s1 defaultConnection {0}@," name_cc mem;
  end;

  if !Esml2vhdl.allow_heap_assign then begin
    fprintf fmt "add_connection %s.wm %s.s1 avalon@," name_cc mem;
    fprintf fmt "set_connection_parameter_value %s.wm/%s.s1 arbitrationPriority {1}@," name_cc mem;
    fprintf fmt "set_connection_parameter_value %s.wm/%s.s1 baseAddress {0x00000000}@," name_cc mem;
    fprintf fmt "set_connection_parameter_value %s.wm/%s.s1 defaultConnection {0}@," name_cc mem;
    end;
  
  fprintf fmt "add_connection clk.clk %s.clock clock@," name_cc;

  fprintf fmt "add_connection clk.clk_reset %s.reset reset@," name_cc;

  fprintf fmt "@."
