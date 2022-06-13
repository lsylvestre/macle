# qsys scripting (.tcl) file for platform
package require -exact qsys 16.0

# create_system {platform}

set_project_property DEVICE_FAMILY {MAX 10}
set_project_property DEVICE {10M50DAF484C6GES}
set_project_property HIDE_FROM_IP_CATALOG {false}

# Instances and instance parameters
# (disabled instances are intentionally culled)
add_instance altpll_0 altpll 20.1
set_instance_parameter_value altpll_0 {AVALON_USE_SEPARATE_SYSCLK} {NO}
set_instance_parameter_value altpll_0 {BANDWIDTH} {}
set_instance_parameter_value altpll_0 {BANDWIDTH_TYPE} {AUTO}
set_instance_parameter_value altpll_0 {CLK0_DIVIDE_BY} {1}
set_instance_parameter_value altpll_0 {CLK0_DUTY_CYCLE} {50}
set_instance_parameter_value altpll_0 {CLK0_MULTIPLY_BY} {2}
set_instance_parameter_value altpll_0 {CLK0_PHASE_SHIFT} {0}
set_instance_parameter_value altpll_0 {CLK1_DIVIDE_BY} {1}
set_instance_parameter_value altpll_0 {CLK1_DUTY_CYCLE} {50}
set_instance_parameter_value altpll_0 {CLK1_MULTIPLY_BY} {2}
set_instance_parameter_value altpll_0 {CLK1_PHASE_SHIFT} {-3000}
set_instance_parameter_value altpll_0 {CLK2_DIVIDE_BY} {}
set_instance_parameter_value altpll_0 {CLK2_DUTY_CYCLE} {}
set_instance_parameter_value altpll_0 {CLK2_MULTIPLY_BY} {}
set_instance_parameter_value altpll_0 {CLK2_PHASE_SHIFT} {}
set_instance_parameter_value altpll_0 {CLK3_DIVIDE_BY} {}
set_instance_parameter_value altpll_0 {CLK3_DUTY_CYCLE} {}
set_instance_parameter_value altpll_0 {CLK3_MULTIPLY_BY} {}
set_instance_parameter_value altpll_0 {CLK3_PHASE_SHIFT} {}
set_instance_parameter_value altpll_0 {CLK4_DIVIDE_BY} {}
set_instance_parameter_value altpll_0 {CLK4_DUTY_CYCLE} {}
set_instance_parameter_value altpll_0 {CLK4_MULTIPLY_BY} {}
set_instance_parameter_value altpll_0 {CLK4_PHASE_SHIFT} {}
set_instance_parameter_value altpll_0 {CLK5_DIVIDE_BY} {}
set_instance_parameter_value altpll_0 {CLK5_DUTY_CYCLE} {}
set_instance_parameter_value altpll_0 {CLK5_MULTIPLY_BY} {}
set_instance_parameter_value altpll_0 {CLK5_PHASE_SHIFT} {}
set_instance_parameter_value altpll_0 {CLK6_DIVIDE_BY} {}
set_instance_parameter_value altpll_0 {CLK6_DUTY_CYCLE} {}
set_instance_parameter_value altpll_0 {CLK6_MULTIPLY_BY} {}
set_instance_parameter_value altpll_0 {CLK6_PHASE_SHIFT} {}
set_instance_parameter_value altpll_0 {CLK7_DIVIDE_BY} {}
set_instance_parameter_value altpll_0 {CLK7_DUTY_CYCLE} {}
set_instance_parameter_value altpll_0 {CLK7_MULTIPLY_BY} {}
set_instance_parameter_value altpll_0 {CLK7_PHASE_SHIFT} {}
set_instance_parameter_value altpll_0 {CLK8_DIVIDE_BY} {}
set_instance_parameter_value altpll_0 {CLK8_DUTY_CYCLE} {}
set_instance_parameter_value altpll_0 {CLK8_MULTIPLY_BY} {}
set_instance_parameter_value altpll_0 {CLK8_PHASE_SHIFT} {}
set_instance_parameter_value altpll_0 {CLK9_DIVIDE_BY} {}
set_instance_parameter_value altpll_0 {CLK9_DUTY_CYCLE} {}
set_instance_parameter_value altpll_0 {CLK9_MULTIPLY_BY} {}
set_instance_parameter_value altpll_0 {CLK9_PHASE_SHIFT} {}
set_instance_parameter_value altpll_0 {COMPENSATE_CLOCK} {CLK0}
set_instance_parameter_value altpll_0 {DOWN_SPREAD} {}
set_instance_parameter_value altpll_0 {DPA_DIVIDER} {}
set_instance_parameter_value altpll_0 {DPA_DIVIDE_BY} {}
set_instance_parameter_value altpll_0 {DPA_MULTIPLY_BY} {}
set_instance_parameter_value altpll_0 {ENABLE_SWITCH_OVER_COUNTER} {}
set_instance_parameter_value altpll_0 {EXTCLK0_DIVIDE_BY} {}
set_instance_parameter_value altpll_0 {EXTCLK0_DUTY_CYCLE} {}
set_instance_parameter_value altpll_0 {EXTCLK0_MULTIPLY_BY} {}
set_instance_parameter_value altpll_0 {EXTCLK0_PHASE_SHIFT} {}
set_instance_parameter_value altpll_0 {EXTCLK1_DIVIDE_BY} {}
set_instance_parameter_value altpll_0 {EXTCLK1_DUTY_CYCLE} {}
set_instance_parameter_value altpll_0 {EXTCLK1_MULTIPLY_BY} {}
set_instance_parameter_value altpll_0 {EXTCLK1_PHASE_SHIFT} {}
set_instance_parameter_value altpll_0 {EXTCLK2_DIVIDE_BY} {}
set_instance_parameter_value altpll_0 {EXTCLK2_DUTY_CYCLE} {}
set_instance_parameter_value altpll_0 {EXTCLK2_MULTIPLY_BY} {}
set_instance_parameter_value altpll_0 {EXTCLK2_PHASE_SHIFT} {}
set_instance_parameter_value altpll_0 {EXTCLK3_DIVIDE_BY} {}
set_instance_parameter_value altpll_0 {EXTCLK3_DUTY_CYCLE} {}
set_instance_parameter_value altpll_0 {EXTCLK3_MULTIPLY_BY} {}
set_instance_parameter_value altpll_0 {EXTCLK3_PHASE_SHIFT} {}
set_instance_parameter_value altpll_0 {FEEDBACK_SOURCE} {}
set_instance_parameter_value altpll_0 {GATE_LOCK_COUNTER} {}
set_instance_parameter_value altpll_0 {GATE_LOCK_SIGNAL} {}
set_instance_parameter_value altpll_0 {HIDDEN_CONSTANTS} {CT#PORT_clk5 PORT_UNUSED CT#PORT_clk4 PORT_UNUSED CT#PORT_clk3 PORT_UNUSED CT#PORT_clk2 PORT_UNUSED CT#PORT_clk1 PORT_USED CT#PORT_clk0 PORT_USED CT#CLK0_MULTIPLY_BY 2 CT#PORT_SCANWRITE PORT_UNUSED CT#PORT_SCANACLR PORT_UNUSED CT#PORT_PFDENA PORT_UNUSED CT#PORT_PLLENA PORT_UNUSED CT#PORT_SCANDATA PORT_UNUSED CT#PORT_SCANCLKENA PORT_UNUSED CT#WIDTH_CLOCK 5 CT#PORT_SCANDATAOUT PORT_UNUSED CT#LPM_TYPE altpll CT#PLL_TYPE AUTO CT#CLK0_PHASE_SHIFT 0 CT#CLK1_DUTY_CYCLE 50 CT#PORT_PHASEDONE PORT_UNUSED CT#OPERATION_MODE NORMAL CT#PORT_CONFIGUPDATE PORT_UNUSED CT#CLK1_MULTIPLY_BY 2 CT#COMPENSATE_CLOCK CLK0 CT#PORT_CLKSWITCH PORT_UNUSED CT#INCLK0_INPUT_FREQUENCY 20000 CT#PORT_SCANDONE PORT_UNUSED CT#PORT_CLKLOSS PORT_UNUSED CT#PORT_INCLK1 PORT_UNUSED CT#AVALON_USE_SEPARATE_SYSCLK NO CT#PORT_INCLK0 PORT_USED CT#PORT_clkena5 PORT_UNUSED CT#PORT_clkena4 PORT_UNUSED CT#PORT_clkena3 PORT_UNUSED CT#PORT_clkena2 PORT_UNUSED CT#PORT_clkena1 PORT_UNUSED CT#PORT_clkena0 PORT_UNUSED CT#CLK1_PHASE_SHIFT -3000 CT#PORT_ARESET PORT_USED CT#BANDWIDTH_TYPE AUTO CT#INTENDED_DEVICE_FAMILY {MAX 10} CT#PORT_SCANREAD PORT_UNUSED CT#PORT_PHASESTEP PORT_UNUSED CT#PORT_SCANCLK PORT_UNUSED CT#PORT_CLKBAD1 PORT_UNUSED CT#PORT_CLKBAD0 PORT_UNUSED CT#PORT_FBIN PORT_UNUSED CT#PORT_PHASEUPDOWN PORT_UNUSED CT#PORT_extclk3 PORT_UNUSED CT#PORT_extclk2 PORT_UNUSED CT#PORT_extclk1 PORT_UNUSED CT#PORT_PHASECOUNTERSELECT PORT_UNUSED CT#PORT_extclk0 PORT_UNUSED CT#PORT_ACTIVECLOCK PORT_UNUSED CT#CLK0_DUTY_CYCLE 50 CT#CLK0_DIVIDE_BY 1 CT#CLK1_DIVIDE_BY 1 CT#PORT_LOCKED PORT_USED}
set_instance_parameter_value altpll_0 {HIDDEN_CUSTOM_ELABORATION} {altpll_avalon_elaboration}
set_instance_parameter_value altpll_0 {HIDDEN_CUSTOM_POST_EDIT} {altpll_avalon_post_edit}
set_instance_parameter_value altpll_0 {HIDDEN_IF_PORTS} {IF#phasecounterselect {input 3} IF#locked {output 0} IF#reset {input 0} IF#clk {input 0} IF#phaseupdown {input 0} IF#scandone {output 0} IF#readdata {output 32} IF#write {input 0} IF#scanclk {input 0} IF#phasedone {output 0} IF#c4 {output 0} IF#c3 {output 0} IF#c2 {output 0} IF#address {input 2} IF#c1 {output 0} IF#c0 {output 0} IF#writedata {input 32} IF#read {input 0} IF#areset {input 0} IF#scanclkena {input 0} IF#scandataout {output 0} IF#configupdate {input 0} IF#phasestep {input 0} IF#scandata {input 0}}
set_instance_parameter_value altpll_0 {HIDDEN_IS_FIRST_EDIT} {0}
set_instance_parameter_value altpll_0 {HIDDEN_IS_NUMERIC} {IN#WIDTH_CLOCK 1 IN#CLK0_DUTY_CYCLE 1 IN#PLL_TARGET_HARCOPY_CHECK 1 IN#CLK1_MULTIPLY_BY 1 IN#SWITCHOVER_COUNT_EDIT 1 IN#INCLK0_INPUT_FREQUENCY 1 IN#PLL_LVDS_PLL_CHECK 1 IN#PLL_AUTOPLL_CHECK 1 IN#PLL_FASTPLL_CHECK 1 IN#CLK1_DUTY_CYCLE 1 IN#PLL_ENHPLL_CHECK 1 IN#DIV_FACTOR1 1 IN#DIV_FACTOR0 1 IN#LVDS_MODE_DATA_RATE_DIRTY 1 IN#GLOCK_COUNTER_EDIT 1 IN#CLK0_DIVIDE_BY 1 IN#MULT_FACTOR1 1 IN#MULT_FACTOR0 1 IN#CLK0_MULTIPLY_BY 1 IN#USE_MIL_SPEED_GRADE 1 IN#CLK1_DIVIDE_BY 1}
set_instance_parameter_value altpll_0 {HIDDEN_MF_PORTS} {MF#areset 1 MF#clk 1 MF#locked 1 MF#inclk 1}
set_instance_parameter_value altpll_0 {HIDDEN_PRIVATES} {PT#GLOCKED_FEATURE_ENABLED 0 PT#SPREAD_FEATURE_ENABLED 0 PT#BANDWIDTH_FREQ_UNIT MHz PT#CUR_DEDICATED_CLK c0 PT#INCLK0_FREQ_EDIT 50.000 PT#BANDWIDTH_PRESET Low PT#PLL_LVDS_PLL_CHECK 0 PT#BANDWIDTH_USE_PRESET 0 PT#AVALON_USE_SEPARATE_SYSCLK NO PT#PLL_ENHPLL_CHECK 0 PT#OUTPUT_FREQ_UNIT1 MHz PT#OUTPUT_FREQ_UNIT0 MHz PT#PHASE_RECONFIG_FEATURE_ENABLED 1 PT#CREATE_CLKBAD_CHECK 0 PT#CLKSWITCH_CHECK 0 PT#INCLK1_FREQ_EDIT 100.000 PT#NORMAL_MODE_RADIO 1 PT#SRC_SYNCH_COMP_RADIO 0 PT#PLL_ARESET_CHECK 1 PT#LONG_SCAN_RADIO 1 PT#SCAN_FEATURE_ENABLED 1 PT#PHASE_RECONFIG_INPUTS_CHECK 0 PT#USE_CLK1 1 PT#USE_CLK0 1 PT#PRIMARY_CLK_COMBO inclk0 PT#BANDWIDTH 1.000 PT#GLOCKED_COUNTER_EDIT_CHANGED 1 PT#PLL_FASTPLL_CHECK 0 PT#SPREAD_FREQ_UNIT KHz PT#PLL_AUTOPLL_CHECK 1 PT#LVDS_PHASE_SHIFT_UNIT1 deg PT#LVDS_PHASE_SHIFT_UNIT0 deg PT#OUTPUT_FREQ_MODE1 0 PT#SWITCHOVER_FEATURE_ENABLED 0 PT#MIG_DEVICE_SPEED_GRADE Any PT#OUTPUT_FREQ_MODE0 1 PT#BANDWIDTH_FEATURE_ENABLED 1 PT#INCLK0_FREQ_UNIT_COMBO MHz PT#ZERO_DELAY_RADIO 0 PT#OUTPUT_FREQ1 100.00000000 PT#OUTPUT_FREQ0 100.00000000 PT#SHORT_SCAN_RADIO 0 PT#LVDS_MODE_DATA_RATE_DIRTY 0 PT#CUR_FBIN_CLK c0 PT#PLL_ADVANCED_PARAM_CHECK 0 PT#CLKBAD_SWITCHOVER_CHECK 0 PT#PHASE_SHIFT_STEP_ENABLED_CHECK 0 PT#DEVICE_SPEED_GRADE Any PT#PLL_FBMIMIC_CHECK 0 PT#LVDS_MODE_DATA_RATE {Not Available} PT#LOCKED_OUTPUT_CHECK 1 PT#SPREAD_PERCENT 0.000 PT#PHASE_SHIFT1 -3.00000000 PT#PHASE_SHIFT0 0.00000000 PT#DIV_FACTOR1 1 PT#DIV_FACTOR0 1 PT#CNX_NO_COMPENSATE_RADIO 0 PT#USE_CLKENA1 0 PT#USE_CLKENA0 0 PT#CREATE_INCLK1_CHECK 0 PT#GLOCK_COUNTER_EDIT 1048575 PT#INCLK1_FREQ_UNIT_COMBO MHz PT#EFF_OUTPUT_FREQ_VALUE1 100.000000 PT#EFF_OUTPUT_FREQ_VALUE0 100.000000 PT#SPREAD_FREQ 50.000 PT#USE_MIL_SPEED_GRADE 0 PT#EXPLICIT_SWITCHOVER_COUNTER 0 PT#STICKY_CLK4 0 PT#STICKY_CLK3 0 PT#STICKY_CLK2 0 PT#STICKY_CLK1 1 PT#STICKY_CLK0 1 PT#EXT_FEEDBACK_RADIO 0 PT#MIRROR_CLK1 0 PT#MIRROR_CLK0 0 PT#SWITCHOVER_COUNT_EDIT 1 PT#SELF_RESET_LOCK_LOSS 0 PT#PLL_PFDENA_CHECK 0 PT#INT_FEEDBACK__MODE_RADIO 1 PT#INCLK1_FREQ_EDIT_CHANGED 1 PT#CLKLOSS_CHECK 0 PT#SYNTH_WRAPPER_GEN_POSTFIX 0 PT#PHASE_SHIFT_UNIT1 ns PT#PHASE_SHIFT_UNIT0 deg PT#BANDWIDTH_USE_AUTO 1 PT#HAS_MANUAL_SWITCHOVER 1 PT#MULT_FACTOR1 2 PT#MULT_FACTOR0 1 PT#SPREAD_USE 0 PT#GLOCKED_MODE_CHECK 0 PT#SACN_INPUTS_CHECK 0 PT#DUTY_CYCLE1 50.00000000 PT#INTENDED_DEVICE_FAMILY {MAX 10} PT#DUTY_CYCLE0 50.00000000 PT#PLL_TARGET_HARCOPY_CHECK 0 PT#INCLK1_FREQ_UNIT_CHANGED 1 PT#RECONFIG_FILE ALTPLL1643189102595771.mif PT#ACTIVECLK_CHECK 0}
set_instance_parameter_value altpll_0 {HIDDEN_USED_PORTS} {UP#locked used UP#c1 used UP#c0 used UP#areset used UP#inclk0 used}
set_instance_parameter_value altpll_0 {INCLK0_INPUT_FREQUENCY} {20000}
set_instance_parameter_value altpll_0 {INCLK1_INPUT_FREQUENCY} {}
set_instance_parameter_value altpll_0 {INTENDED_DEVICE_FAMILY} {MAX 10}
set_instance_parameter_value altpll_0 {INVALID_LOCK_MULTIPLIER} {}
set_instance_parameter_value altpll_0 {LOCK_HIGH} {}
set_instance_parameter_value altpll_0 {LOCK_LOW} {}
set_instance_parameter_value altpll_0 {OPERATION_MODE} {NORMAL}
set_instance_parameter_value altpll_0 {PLL_TYPE} {AUTO}
set_instance_parameter_value altpll_0 {PORT_ACTIVECLOCK} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_ARESET} {PORT_USED}
set_instance_parameter_value altpll_0 {PORT_CLKBAD0} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_CLKBAD1} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_CLKLOSS} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_CLKSWITCH} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_CONFIGUPDATE} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_ENABLE0} {}
set_instance_parameter_value altpll_0 {PORT_ENABLE1} {}
set_instance_parameter_value altpll_0 {PORT_FBIN} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_FBOUT} {}
set_instance_parameter_value altpll_0 {PORT_INCLK0} {PORT_USED}
set_instance_parameter_value altpll_0 {PORT_INCLK1} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_LOCKED} {PORT_USED}
set_instance_parameter_value altpll_0 {PORT_PFDENA} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_PHASECOUNTERSELECT} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_PHASEDONE} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_PHASESTEP} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_PHASEUPDOWN} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_PLLENA} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_SCANACLR} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_SCANCLK} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_SCANCLKENA} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_SCANDATA} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_SCANDATAOUT} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_SCANDONE} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_SCANREAD} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_SCANWRITE} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_SCLKOUT0} {}
set_instance_parameter_value altpll_0 {PORT_SCLKOUT1} {}
set_instance_parameter_value altpll_0 {PORT_VCOOVERRANGE} {}
set_instance_parameter_value altpll_0 {PORT_VCOUNDERRANGE} {}
set_instance_parameter_value altpll_0 {PORT_clk0} {PORT_USED}
set_instance_parameter_value altpll_0 {PORT_clk1} {PORT_USED}
set_instance_parameter_value altpll_0 {PORT_clk2} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_clk3} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_clk4} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_clk5} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_clk6} {}
set_instance_parameter_value altpll_0 {PORT_clk7} {}
set_instance_parameter_value altpll_0 {PORT_clk8} {}
set_instance_parameter_value altpll_0 {PORT_clk9} {}
set_instance_parameter_value altpll_0 {PORT_clkena0} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_clkena1} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_clkena2} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_clkena3} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_clkena4} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_clkena5} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_extclk0} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_extclk1} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_extclk2} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_extclk3} {PORT_UNUSED}
set_instance_parameter_value altpll_0 {PORT_extclkena0} {}
set_instance_parameter_value altpll_0 {PORT_extclkena1} {}
set_instance_parameter_value altpll_0 {PORT_extclkena2} {}
set_instance_parameter_value altpll_0 {PORT_extclkena3} {}
set_instance_parameter_value altpll_0 {PRIMARY_CLOCK} {}
set_instance_parameter_value altpll_0 {QUALIFY_CONF_DONE} {}
set_instance_parameter_value altpll_0 {SCAN_CHAIN} {}
set_instance_parameter_value altpll_0 {SCAN_CHAIN_MIF_FILE} {}
set_instance_parameter_value altpll_0 {SCLKOUT0_PHASE_SHIFT} {}
set_instance_parameter_value altpll_0 {SCLKOUT1_PHASE_SHIFT} {}
set_instance_parameter_value altpll_0 {SELF_RESET_ON_GATED_LOSS_LOCK} {}
set_instance_parameter_value altpll_0 {SELF_RESET_ON_LOSS_LOCK} {}
set_instance_parameter_value altpll_0 {SKIP_VCO} {}
set_instance_parameter_value altpll_0 {SPREAD_FREQUENCY} {}
set_instance_parameter_value altpll_0 {SWITCH_OVER_COUNTER} {}
set_instance_parameter_value altpll_0 {SWITCH_OVER_ON_GATED_LOCK} {}
set_instance_parameter_value altpll_0 {SWITCH_OVER_ON_LOSSCLK} {}
set_instance_parameter_value altpll_0 {SWITCH_OVER_TYPE} {}
set_instance_parameter_value altpll_0 {USING_FBMIMICBIDIR_PORT} {}
set_instance_parameter_value altpll_0 {VALID_LOCK_MULTIPLIER} {}
set_instance_parameter_value altpll_0 {VCO_DIVIDE_BY} {}
set_instance_parameter_value altpll_0 {VCO_FREQUENCY_CONTROL} {}
set_instance_parameter_value altpll_0 {VCO_MULTIPLY_BY} {}
set_instance_parameter_value altpll_0 {VCO_PHASE_SHIFT_STEP} {}
set_instance_parameter_value altpll_0 {WIDTH_CLOCK} {5}
set_instance_parameter_value altpll_0 {WIDTH_PHASECOUNTERSELECT} {}

add_instance clk clock_source 20.1
set_instance_parameter_value clk {clockFrequency} {50000000.0}
set_instance_parameter_value clk {clockFrequencyKnown} {1}
set_instance_parameter_value clk {resetSynchronousEdges} {NONE}

add_instance cpu altera_nios2_gen2 20.1
set_instance_parameter_value cpu {bht_ramBlockType} {Automatic}
set_instance_parameter_value cpu {breakOffset} {32}
set_instance_parameter_value cpu {breakSlave} {None}
set_instance_parameter_value cpu {cdx_enabled} {0}
set_instance_parameter_value cpu {cpuArchRev} {1}
set_instance_parameter_value cpu {cpuID} {0}
set_instance_parameter_value cpu {cpuReset} {0}
set_instance_parameter_value cpu {data_master_high_performance_paddr_base} {0}
set_instance_parameter_value cpu {data_master_high_performance_paddr_size} {0.0}
set_instance_parameter_value cpu {data_master_paddr_base} {0}
set_instance_parameter_value cpu {data_master_paddr_size} {0.0}
set_instance_parameter_value cpu {dcache_bursts} {false}
set_instance_parameter_value cpu {dcache_numTCDM} {0}
set_instance_parameter_value cpu {dcache_ramBlockType} {Automatic}
set_instance_parameter_value cpu {dcache_size} {2048}
set_instance_parameter_value cpu {dcache_tagramBlockType} {Automatic}
set_instance_parameter_value cpu {dcache_victim_buf_impl} {ram}
set_instance_parameter_value cpu {debug_OCIOnchipTrace} {_128}
set_instance_parameter_value cpu {debug_assignJtagInstanceID} {0}
set_instance_parameter_value cpu {debug_datatrigger} {0}
set_instance_parameter_value cpu {debug_debugReqSignals} {0}
set_instance_parameter_value cpu {debug_enabled} {1}
set_instance_parameter_value cpu {debug_hwbreakpoint} {0}
set_instance_parameter_value cpu {debug_jtagInstanceID} {0}
set_instance_parameter_value cpu {debug_traceStorage} {onchip_trace}
set_instance_parameter_value cpu {debug_traceType} {none}
set_instance_parameter_value cpu {debug_triggerArming} {1}
set_instance_parameter_value cpu {dividerType} {no_div}
set_instance_parameter_value cpu {exceptionOffset} {32}
set_instance_parameter_value cpu {exceptionSlave} {onchip_memory.s1}
set_instance_parameter_value cpu {fa_cache_line} {2}
set_instance_parameter_value cpu {fa_cache_linesize} {0}
set_instance_parameter_value cpu {flash_instruction_master_paddr_base} {0}
set_instance_parameter_value cpu {flash_instruction_master_paddr_size} {0.0}
set_instance_parameter_value cpu {icache_burstType} {None}
set_instance_parameter_value cpu {icache_numTCIM} {0}
set_instance_parameter_value cpu {icache_ramBlockType} {Automatic}
set_instance_parameter_value cpu {icache_size} {4096}
set_instance_parameter_value cpu {icache_tagramBlockType} {Automatic}
set_instance_parameter_value cpu {impl} {Tiny}
set_instance_parameter_value cpu {instruction_master_high_performance_paddr_base} {0}
set_instance_parameter_value cpu {instruction_master_high_performance_paddr_size} {0.0}
set_instance_parameter_value cpu {instruction_master_paddr_base} {0}
set_instance_parameter_value cpu {instruction_master_paddr_size} {0.0}
set_instance_parameter_value cpu {io_regionbase} {0}
set_instance_parameter_value cpu {io_regionsize} {0}
set_instance_parameter_value cpu {master_addr_map} {0}
set_instance_parameter_value cpu {mmu_TLBMissExcOffset} {0}
set_instance_parameter_value cpu {mmu_TLBMissExcSlave} {None}
set_instance_parameter_value cpu {mmu_autoAssignTlbPtrSz} {1}
set_instance_parameter_value cpu {mmu_enabled} {0}
set_instance_parameter_value cpu {mmu_processIDNumBits} {8}
set_instance_parameter_value cpu {mmu_ramBlockType} {Automatic}
set_instance_parameter_value cpu {mmu_tlbNumWays} {16}
set_instance_parameter_value cpu {mmu_tlbPtrSz} {7}
set_instance_parameter_value cpu {mmu_udtlbNumEntries} {6}
set_instance_parameter_value cpu {mmu_uitlbNumEntries} {4}
set_instance_parameter_value cpu {mpu_enabled} {0}
set_instance_parameter_value cpu {mpu_minDataRegionSize} {12}
set_instance_parameter_value cpu {mpu_minInstRegionSize} {12}
set_instance_parameter_value cpu {mpu_numOfDataRegion} {8}
set_instance_parameter_value cpu {mpu_numOfInstRegion} {8}
set_instance_parameter_value cpu {mpu_useLimit} {0}
set_instance_parameter_value cpu {mpx_enabled} {0}
set_instance_parameter_value cpu {mul_32_impl} {2}
set_instance_parameter_value cpu {mul_64_impl} {0}
set_instance_parameter_value cpu {mul_shift_choice} {0}
set_instance_parameter_value cpu {ocimem_ramBlockType} {Automatic}
set_instance_parameter_value cpu {ocimem_ramInit} {0}
set_instance_parameter_value cpu {regfile_ramBlockType} {Automatic}
set_instance_parameter_value cpu {register_file_por} {0}
set_instance_parameter_value cpu {resetOffset} {0}
set_instance_parameter_value cpu {resetSlave} {onchip_memory.s1}
set_instance_parameter_value cpu {resetrequest_enabled} {1}
set_instance_parameter_value cpu {setting_HBreakTest} {0}
set_instance_parameter_value cpu {setting_HDLSimCachesCleared} {1}
set_instance_parameter_value cpu {setting_activateMonitors} {1}
set_instance_parameter_value cpu {setting_activateTestEndChecker} {0}
set_instance_parameter_value cpu {setting_activateTrace} {0}
set_instance_parameter_value cpu {setting_allow_break_inst} {0}
set_instance_parameter_value cpu {setting_alwaysEncrypt} {1}
set_instance_parameter_value cpu {setting_asic_add_scan_mode_input} {0}
set_instance_parameter_value cpu {setting_asic_enabled} {0}
set_instance_parameter_value cpu {setting_asic_synopsys_translate_on_off} {0}
set_instance_parameter_value cpu {setting_asic_third_party_synthesis} {0}
set_instance_parameter_value cpu {setting_avalonDebugPortPresent} {0}
set_instance_parameter_value cpu {setting_bhtPtrSz} {8}
set_instance_parameter_value cpu {setting_bigEndian} {0}
set_instance_parameter_value cpu {setting_branchpredictiontype} {Dynamic}
set_instance_parameter_value cpu {setting_breakslaveoveride} {0}
set_instance_parameter_value cpu {setting_clearXBitsLDNonBypass} {1}
set_instance_parameter_value cpu {setting_dc_ecc_present} {1}
set_instance_parameter_value cpu {setting_disable_tmr_inj} {0}
set_instance_parameter_value cpu {setting_disableocitrace} {0}
set_instance_parameter_value cpu {setting_dtcm_ecc_present} {1}
set_instance_parameter_value cpu {setting_ecc_present} {0}
set_instance_parameter_value cpu {setting_ecc_sim_test_ports} {0}
set_instance_parameter_value cpu {setting_exportHostDebugPort} {0}
set_instance_parameter_value cpu {setting_exportPCB} {0}
set_instance_parameter_value cpu {setting_export_large_RAMs} {0}
set_instance_parameter_value cpu {setting_exportdebuginfo} {0}
set_instance_parameter_value cpu {setting_exportvectors} {0}
set_instance_parameter_value cpu {setting_fast_register_read} {0}
set_instance_parameter_value cpu {setting_ic_ecc_present} {1}
set_instance_parameter_value cpu {setting_interruptControllerType} {Internal}
set_instance_parameter_value cpu {setting_itcm_ecc_present} {1}
set_instance_parameter_value cpu {setting_mmu_ecc_present} {1}
set_instance_parameter_value cpu {setting_oci_export_jtag_signals} {0}
set_instance_parameter_value cpu {setting_oci_version} {1}
set_instance_parameter_value cpu {setting_preciseIllegalMemAccessException} {0}
set_instance_parameter_value cpu {setting_removeRAMinit} {0}
set_instance_parameter_value cpu {setting_rf_ecc_present} {1}
set_instance_parameter_value cpu {setting_shadowRegisterSets} {0}
set_instance_parameter_value cpu {setting_showInternalSettings} {0}
set_instance_parameter_value cpu {setting_showUnpublishedSettings} {0}
set_instance_parameter_value cpu {setting_support31bitdcachebypass} {1}
set_instance_parameter_value cpu {setting_tmr_output_disable} {0}
set_instance_parameter_value cpu {setting_usedesignware} {0}
set_instance_parameter_value cpu {shift_rot_impl} {1}
set_instance_parameter_value cpu {tightly_coupled_data_master_0_paddr_base} {0}
set_instance_parameter_value cpu {tightly_coupled_data_master_0_paddr_size} {0.0}
set_instance_parameter_value cpu {tightly_coupled_data_master_1_paddr_base} {0}
set_instance_parameter_value cpu {tightly_coupled_data_master_1_paddr_size} {0.0}
set_instance_parameter_value cpu {tightly_coupled_data_master_2_paddr_base} {0}
set_instance_parameter_value cpu {tightly_coupled_data_master_2_paddr_size} {0.0}
set_instance_parameter_value cpu {tightly_coupled_data_master_3_paddr_base} {0}
set_instance_parameter_value cpu {tightly_coupled_data_master_3_paddr_size} {0.0}
set_instance_parameter_value cpu {tightly_coupled_instruction_master_0_paddr_base} {0}
set_instance_parameter_value cpu {tightly_coupled_instruction_master_0_paddr_size} {0.0}
set_instance_parameter_value cpu {tightly_coupled_instruction_master_1_paddr_base} {0}
set_instance_parameter_value cpu {tightly_coupled_instruction_master_1_paddr_size} {0.0}
set_instance_parameter_value cpu {tightly_coupled_instruction_master_2_paddr_base} {0}
set_instance_parameter_value cpu {tightly_coupled_instruction_master_2_paddr_size} {0.0}
set_instance_parameter_value cpu {tightly_coupled_instruction_master_3_paddr_base} {0}
set_instance_parameter_value cpu {tightly_coupled_instruction_master_3_paddr_size} {0.0}
set_instance_parameter_value cpu {tmr_enabled} {0}
set_instance_parameter_value cpu {tracefilename} {}
set_instance_parameter_value cpu {userDefinedSettings} {}

add_instance jtag_uart altera_avalon_jtag_uart 20.1
set_instance_parameter_value jtag_uart {allowMultipleConnections} {0}
set_instance_parameter_value jtag_uart {hubInstanceID} {0}
set_instance_parameter_value jtag_uart {readBufferDepth} {64}
set_instance_parameter_value jtag_uart {readIRQThreshold} {8}
set_instance_parameter_value jtag_uart {simInputCharacterStream} {}
set_instance_parameter_value jtag_uart {simInteractiveOptions} {NO_INTERACTIVE_WINDOWS}
set_instance_parameter_value jtag_uart {useRegistersForReadBuffer} {0}
set_instance_parameter_value jtag_uart {useRegistersForWriteBuffer} {0}
set_instance_parameter_value jtag_uart {useRelativePathForSimFile} {0}
set_instance_parameter_value jtag_uart {writeBufferDepth} {64}
set_instance_parameter_value jtag_uart {writeIRQThreshold} {8}

add_instance onchip_memory altera_avalon_onchip_memory2 20.1
set_instance_parameter_value onchip_memory {allowInSystemMemoryContentEditor} {0}
set_instance_parameter_value onchip_memory {blockType} {AUTO}
set_instance_parameter_value onchip_memory {copyInitFile} {0}
set_instance_parameter_value onchip_memory {dataWidth} {32}
set_instance_parameter_value onchip_memory {dataWidth2} {32}
set_instance_parameter_value onchip_memory {dualPort} {0}
set_instance_parameter_value onchip_memory {ecc_enabled} {0}
set_instance_parameter_value onchip_memory {enPRInitMode} {0}
set_instance_parameter_value onchip_memory {enableDiffWidth} {0}
set_instance_parameter_value onchip_memory {initMemContent} {0}
set_instance_parameter_value onchip_memory {initializationFileName} {onchip_mem.hex}
set_instance_parameter_value onchip_memory {instanceID} {NONE}
set_instance_parameter_value onchip_memory {memorySize} {131072.0}
set_instance_parameter_value onchip_memory {readDuringWriteMode} {DONT_CARE}
set_instance_parameter_value onchip_memory {resetrequest_enabled} {1}
set_instance_parameter_value onchip_memory {simAllowMRAMContentsFile} {0}
set_instance_parameter_value onchip_memory {simMemInitOnlyFilename} {0}
set_instance_parameter_value onchip_memory {singleClockOperation} {0}
set_instance_parameter_value onchip_memory {slave1Latency} {1}
set_instance_parameter_value onchip_memory {slave2Latency} {1}
set_instance_parameter_value onchip_memory {useNonDefaultInitFile} {0}
set_instance_parameter_value onchip_memory {useShallowMemBlocks} {0}
set_instance_parameter_value onchip_memory {writable} {1}

add_instance sdram altera_avalon_new_sdram_controller 20.1
set_instance_parameter_value sdram {TAC} {5.4}
set_instance_parameter_value sdram {TMRD} {3.0}
set_instance_parameter_value sdram {TRCD} {15.0}
set_instance_parameter_value sdram {TRFC} {70.0}
set_instance_parameter_value sdram {TRP} {15.0}
set_instance_parameter_value sdram {TWR} {14.0}
set_instance_parameter_value sdram {casLatency} {3}
set_instance_parameter_value sdram {columnWidth} {10}
set_instance_parameter_value sdram {dataWidth} {16}
set_instance_parameter_value sdram {generateSimulationModel} {0}
set_instance_parameter_value sdram {initNOPDelay} {0.0}
set_instance_parameter_value sdram {initRefreshCommands} {2}
set_instance_parameter_value sdram {masteredTristateBridgeSlave} {0}
set_instance_parameter_value sdram {model} {single_Micron_MT48LC4M32B2_7_chip}
set_instance_parameter_value sdram {numberOfBanks} {4}
set_instance_parameter_value sdram {numberOfChipSelects} {1}
set_instance_parameter_value sdram {pinsSharedViaTriState} {0}
set_instance_parameter_value sdram {powerUpDelay} {100.0}
set_instance_parameter_value sdram {refreshPeriod} {7.8125}
set_instance_parameter_value sdram {registerDataIn} {1}
set_instance_parameter_value sdram {rowWidth} {13}

add_instance sys_timer altera_avalon_timer 20.1
set_instance_parameter_value sys_timer {alwaysRun} {0}
set_instance_parameter_value sys_timer {counterSize} {32}
set_instance_parameter_value sys_timer {fixedPeriod} {0}
set_instance_parameter_value sys_timer {period} {1}
set_instance_parameter_value sys_timer {periodUnits} {MSEC}
set_instance_parameter_value sys_timer {resetOutput} {0}
set_instance_parameter_value sys_timer {snapshot} {1}
set_instance_parameter_value sys_timer {timeoutPulseOutput} {0}
set_instance_parameter_value sys_timer {watchdogPulse} {2}

# exported interfaces
add_interface altpll_0_areset_conduit conduit end
set_interface_property altpll_0_areset_conduit EXPORT_OF altpll_0.areset_conduit
add_interface altpll_0_locked_conduit conduit end
set_interface_property altpll_0_locked_conduit EXPORT_OF altpll_0.locked_conduit
add_interface clk clock sink
set_interface_property clk EXPORT_OF clk.clk_in
add_interface reset reset sink
set_interface_property reset EXPORT_OF clk.clk_in_reset
add_interface sdram_clk clock source
set_interface_property sdram_clk EXPORT_OF altpll_0.c1
add_interface sdram_wire conduit end
set_interface_property sdram_wire EXPORT_OF sdram.wire

# connections and connection parameters
add_connection altpll_0.c0 cpu.clk

add_connection altpll_0.c0 jtag_uart.clk

add_connection altpll_0.c0 onchip_memory.clk1

add_connection altpll_0.c0 sdram.clk

add_connection altpll_0.c0 sys_timer.clk

add_connection clk.clk altpll_0.inclk_interface

add_connection clk.clk_reset altpll_0.inclk_interface_reset

add_connection clk.clk_reset cpu.reset

add_connection clk.clk_reset jtag_uart.reset

add_connection clk.clk_reset onchip_memory.reset1

add_connection clk.clk_reset sdram.reset

add_connection clk.clk_reset sys_timer.reset

add_connection cpu.data_master altpll_0.pll_slave
set_connection_parameter_value cpu.data_master/altpll_0.pll_slave arbitrationPriority {1}
set_connection_parameter_value cpu.data_master/altpll_0.pll_slave baseAddress {0x04041020}
set_connection_parameter_value cpu.data_master/altpll_0.pll_slave defaultConnection {0}

add_connection cpu.data_master cpu.debug_mem_slave
set_connection_parameter_value cpu.data_master/cpu.debug_mem_slave arbitrationPriority {1}
set_connection_parameter_value cpu.data_master/cpu.debug_mem_slave baseAddress {0x04040800}
set_connection_parameter_value cpu.data_master/cpu.debug_mem_slave defaultConnection {0}

add_connection cpu.data_master jtag_uart.avalon_jtag_slave
set_connection_parameter_value cpu.data_master/jtag_uart.avalon_jtag_slave arbitrationPriority {1}
set_connection_parameter_value cpu.data_master/jtag_uart.avalon_jtag_slave baseAddress {0x04041030}
set_connection_parameter_value cpu.data_master/jtag_uart.avalon_jtag_slave defaultConnection {0}

add_connection cpu.data_master onchip_memory.s1
set_connection_parameter_value cpu.data_master/onchip_memory.s1 arbitrationPriority {1}
set_connection_parameter_value cpu.data_master/onchip_memory.s1 baseAddress {0x04020000}
set_connection_parameter_value cpu.data_master/onchip_memory.s1 defaultConnection {0}

add_connection cpu.data_master sdram.s1
set_connection_parameter_value cpu.data_master/sdram.s1 arbitrationPriority {1}
set_connection_parameter_value cpu.data_master/sdram.s1 baseAddress {0x0000}
set_connection_parameter_value cpu.data_master/sdram.s1 defaultConnection {0}

add_connection cpu.data_master sys_timer.s1
set_connection_parameter_value cpu.data_master/sys_timer.s1 arbitrationPriority {1}
set_connection_parameter_value cpu.data_master/sys_timer.s1 baseAddress {0x04041000}
set_connection_parameter_value cpu.data_master/sys_timer.s1 defaultConnection {0}

add_connection cpu.instruction_master cpu.debug_mem_slave
set_connection_parameter_value cpu.instruction_master/cpu.debug_mem_slave arbitrationPriority {1}
set_connection_parameter_value cpu.instruction_master/cpu.debug_mem_slave baseAddress {0x04040800}
set_connection_parameter_value cpu.instruction_master/cpu.debug_mem_slave defaultConnection {0}

add_connection cpu.instruction_master onchip_memory.s1
set_connection_parameter_value cpu.instruction_master/onchip_memory.s1 arbitrationPriority {1}
set_connection_parameter_value cpu.instruction_master/onchip_memory.s1 baseAddress {0x04020000}
set_connection_parameter_value cpu.instruction_master/onchip_memory.s1 defaultConnection {0}

add_connection cpu.irq jtag_uart.irq
set_connection_parameter_value cpu.irq/jtag_uart.irq irqNumber {0}

add_connection cpu.irq sys_timer.irq
set_connection_parameter_value cpu.irq/sys_timer.irq irqNumber {1}

# interconnect requirements
set_interconnect_requirement {$system} {qsys_mm.clockCrossingAdapter} {HANDSHAKE}
set_interconnect_requirement {$system} {qsys_mm.enableEccProtection} {FALSE}
set_interconnect_requirement {$system} {qsys_mm.insertDefaultSlave} {FALSE}
set_interconnect_requirement {$system} {qsys_mm.maxAdditionalLatency} {1}

# save_system {platform.qsys}
