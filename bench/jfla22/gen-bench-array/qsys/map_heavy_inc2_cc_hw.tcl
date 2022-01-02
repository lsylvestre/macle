# TCL File Generated __for__ Component Editor 15.1

# map_heavy_inc2_cc "map_heavy_inc2_cc"

# request TCL package from ACDS 15.1
#
package require -exact qsys 15.1

#
#module MAP_HEAVY_INC2_CC
#
set_module_property DESCRIPTION ""
set_module_property NAME MAP_HEAVY_INC2_CC
set_module_property VERSION 1.0
set_module_property INTERNAL false
set_module_property OPAQUE_ADDRESS_MAP true
set_module_property GROUP my_ips
set_module_property AUTHOR ""
set_module_property DISPLAY_NAME MAP_HEAVY_INC2_CC
set_module_property INSTANTIATE_IN_SYSTEM_MODULE true
set_module_property EDITABLE true
set_module_property REPORT_TO_TALKBACK false
set_module_property ALLOW_GREYBOX_GENERATION false
set_module_property REPORT_HIERARCHY false

#
# file sets
#
add_fileset QUARTUS_SYNTH QUARTUS_SYNTH "" ""
set_fileset_property QUARTUS_SYNTH TOP_LEVEL avs_map_heavy_inc2
set_fileset_property QUARTUS_SYNTH ENABLE_RELATIVE_INCLUDE_PATHS false
set_fileset_property QUARTUS_SYNTH ENABLE_FILE_OVERWRITE_MODE false
add_fileset_file map_heavy_inc2_cc.vhd VHDL PATH ../rtl/map_heavy_inc2_cc.vhd TOP_LEVEL_FILE
add_fileset_file map_heavy_inc2.vhd VHDL PATH ../rtl/map_heavy_inc2.vhd
add_fileset_file map_heavy_inc2_misc.vhd VHDL PATH ../rtl/misc/map_heavy_inc2_misc.vhd

#

# parameters
#



#

# display items
#



#

# connection point s0
#

add_interface s0 avalon end
set_interface_property s0 addressUnits WORDS
set_interface_property s0 associatedClock clock
set_interface_property s0 associatedReset reset
set_interface_property s0 bitsPerSymbol 8
set_interface_property s0 burstOnBurstBoundariesOnly false
set_interface_property s0 burstcountUnits WORDS
set_interface_property s0 explicitAddressSpan 0
set_interface_property s0 holdTime 0
set_interface_property s0 linewrapBursts false
set_interface_property s0 maximumPendingReadTransactions 0
set_interface_property s0 maximumPendingWriteTransactions 0
set_interface_property s0 readLatency 0
set_interface_property s0 readWaitTime 1
set_interface_property s0 setupTime 0
set_interface_property s0 timingUnits Cycles
set_interface_property s0 writeWaitTime 0
set_interface_property s0 ENABLED true
set_interface_property s0 EXPORT_OF ""
set_interface_property s0 PORT_NAME_MAP ""
set_interface_property s0 CMSIS_SVD_VARIABLES ""
set_interface_property s0 SVD_ADDRESS_GROUP ""

add_interface_port s0 avs_s0_address address Input 4
add_interface_port s0 avs_s0_write write Input 1
add_interface_port s0 avs_s0_writedata writedata Input 32
add_interface_port s0 avs_s0_read read Input 1
add_interface_port s0 avs_s0_readdata readdata Output 32
set_interface_assignment s0 embeddedsw.configuration.isFlash 0
set_interface_assignment s0 embeddedsw.configuration.isMemoryDevice 0
set_interface_assignment s0 embeddedsw.configuration.isNonVolatileStorage 0
set_interface_assignment s0 embeddedsw.configuration.isPrintableDevice 0


#

# connection point rm
#

add_interface rm avalon start
set_interface_property rm addressUnits SYMBOLS
set_interface_property rm associatedClock clock
set_interface_property rm associatedReset reset
set_interface_property rm bitsPerSymbol 8
set_interface_property rm burstOnBurstBoundariesOnly false
set_interface_property rm burstcountUnits WORDS
set_interface_property rm doStreamReads false
set_interface_property rm doStreamWrites false
set_interface_property rm holdTime 0
set_interface_property rm linewrapBursts false
set_interface_property rm maximumPendingReadTransactions 0
set_interface_property rm maximumPendingWriteTransactions 0
set_interface_property rm readLatency 0
set_interface_property rm readWaitTime 1
set_interface_property rm setupTime 0
set_interface_property rm timingUnits Cycles
set_interface_property rm writeWaitTime 0
set_interface_property rm ENABLED true
set_interface_property rm EXPORT_OF ""
set_interface_property rm PORT_NAME_MAP ""
set_interface_property rm CMSIS_SVD_VARIABLES ""
set_interface_property rm SVD_ADDRESS_GROUP ""

add_interface_port rm avm_rm_address address Output 32
add_interface_port rm avm_rm_read read Output 1
add_interface_port rm avm_rm_readdata readdata Input 32
add_interface_port rm avm_rm_waitrequest waitrequest Input 1


#

# connection point wm
#

add_interface wm avalon start
set_interface_property wm addressUnits SYMBOLS
set_interface_property wm associatedClock clock
set_interface_property wm associatedReset reset
set_interface_property wm bitsPerSymbol 8
set_interface_property wm burstOnBurstBoundariesOnly false
set_interface_property wm burstcountUnits WORDS
set_interface_property wm doStreamReads false
set_interface_property wm doStreamWrites false
set_interface_property wm holdTime 0
set_interface_property wm linewrapBursts false
set_interface_property wm maximumPendingReadTransactions 0
set_interface_property wm maximumPendingWriteTransactions 0
set_interface_property wm readLatency 0
set_interface_property wm readWaitTime 1
set_interface_property wm setupTime 0
set_interface_property wm timingUnits Cycles
set_interface_property wm writeWaitTime 0
set_interface_property wm ENABLED true
set_interface_property wm EXPORT_OF ""
set_interface_property wm PORT_NAME_MAP ""
set_interface_property wm CMSIS_SVD_VARIABLES ""
set_interface_property wm SVD_ADDRESS_GROUP ""

add_interface_port wm avm_wm_address address Output 32
add_interface_port wm avm_wm_write write Output 1
add_interface_port wm avm_wm_writedata writedata Output 32
add_interface_port wm avm_wm_waitrequest waitrequest Input 1


#

# connection point clock
#
add_interface clock clock end
set_interface_property clock clockRate 0
set_interface_property clock ENABLED true
set_interface_property clock EXPORT_OF ""
set_interface_property clock PORT_NAME_MAP ""
set_interface_property clock CMSIS_SVD_VARIABLES ""
set_interface_property clock SVD_ADDRESS_GROUP ""

add_interface_port clock clock_clk clk Input 1

#

# connection point reset
#
add_interface reset reset end
set_interface_property reset associatedClock clock
set_interface_property reset synchronousEdges DEASSERT
set_interface_property reset ENABLED true
set_interface_property reset EXPORT_OF ""
set_interface_property reset PORT_NAME_MAP ""
set_interface_property reset CMSIS_SVD_VARIABLES ""
set_interface_property reset SVD_ADDRESS_GROUP ""

add_interface_port reset reset_reset reset Input 1

