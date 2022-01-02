package require -exact qsys 15.1

# module properties
set_module_property NAME {platform_export}
set_module_property DISPLAY_NAME {platform_export_display}

# default module properties
set_module_property VERSION {1.0}
set_module_property GROUP {default group}
set_module_property DESCRIPTION {default description}
set_module_property AUTHOR {author}

set_module_property COMPOSITION_CALLBACK compose
set_module_property opaque_address_map false

#proc compose { } {
    # Instances and instance parameters
    # (disabled instances are intentionally culled)
    add_instance clk clock_source 15.1
    set_instance_parameter_value clk {clockFrequency} {50000000.0}
    set_instance_parameter_value clk {clockFrequencyKnown} {1}
    set_instance_parameter_value clk {resetSynchronousEdges} {NONE}

    add_instance cpu altera_nios2_gen2 15.1
    set_instance_parameter_value cpu {tmr_enabled} {0}
    set_instance_parameter_value cpu {setting_disable_tmr_inj} {0}
    set_instance_parameter_value cpu {setting_showUnpublishedSettings} {0}
    set_instance_parameter_value cpu {setting_showInternalSettings} {0}
    set_instance_parameter_value cpu {setting_preciseIllegalMemAccessException} {0}
    set_instance_parameter_value cpu {setting_exportPCB} {0}
    set_instance_parameter_value cpu {setting_exportdebuginfo} {0}
    set_instance_parameter_value cpu {setting_clearXBitsLDNonBypass} {1}
    set_instance_parameter_value cpu {setting_bigEndian} {0}
    set_instance_parameter_value cpu {setting_export_large_RAMs} {0}
    set_instance_parameter_value cpu {setting_asic_enabled} {0}
    set_instance_parameter_value cpu {setting_asic_synopsys_translate_on_off} {0}
    set_instance_parameter_value cpu {setting_asic_third_party_synthesis} {0}
    set_instance_parameter_value cpu {setting_asic_add_scan_mode_input} {0}
    set_instance_parameter_value cpu {setting_oci_version} {1}
    set_instance_parameter_value cpu {setting_fast_register_read} {0}
    set_instance_parameter_value cpu {setting_exportHostDebugPort} {0}
    set_instance_parameter_value cpu {setting_oci_export_jtag_signals} {0}
    set_instance_parameter_value cpu {setting_avalonDebugPortPresent} {0}
    set_instance_parameter_value cpu {setting_alwaysEncrypt} {1}
    set_instance_parameter_value cpu {io_regionbase} {0}
    set_instance_parameter_value cpu {io_regionsize} {0}
    set_instance_parameter_value cpu {setting_support31bitdcachebypass} {1}
    set_instance_parameter_value cpu {setting_activateTrace} {0}
    set_instance_parameter_value cpu {setting_allow_break_inst} {0}
    set_instance_parameter_value cpu {setting_activateTestEndChecker} {0}
    set_instance_parameter_value cpu {setting_ecc_sim_test_ports} {0}
    set_instance_parameter_value cpu {setting_disableocitrace} {0}
    set_instance_parameter_value cpu {setting_activateMonitors} {1}
    set_instance_parameter_value cpu {setting_HDLSimCachesCleared} {1}
    set_instance_parameter_value cpu {setting_HBreakTest} {0}
    set_instance_parameter_value cpu {setting_breakslaveoveride} {0}
    set_instance_parameter_value cpu {mpu_useLimit} {0}
    set_instance_parameter_value cpu {mpu_enabled} {0}
    set_instance_parameter_value cpu {mmu_enabled} {0}
    set_instance_parameter_value cpu {mmu_autoAssignTlbPtrSz} {1}
    set_instance_parameter_value cpu {cpuReset} {0}
    set_instance_parameter_value cpu {resetrequest_enabled} {1}
    set_instance_parameter_value cpu {setting_removeRAMinit} {0}
    set_instance_parameter_value cpu {setting_shadowRegisterSets} {0}
    set_instance_parameter_value cpu {mpu_numOfInstRegion} {8}
    set_instance_parameter_value cpu {mpu_numOfDataRegion} {8}
    set_instance_parameter_value cpu {mmu_TLBMissExcOffset} {0}
    set_instance_parameter_value cpu {resetOffset} {0}
    set_instance_parameter_value cpu {exceptionOffset} {32}
    set_instance_parameter_value cpu {cpuID} {0}
    set_instance_parameter_value cpu {breakOffset} {32}
    set_instance_parameter_value cpu {userDefinedSettings} {}
    set_instance_parameter_value cpu {tracefilename} {}
    set_instance_parameter_value cpu {resetSlave} {onchip_memory.s1}
    set_instance_parameter_value cpu {mmu_TLBMissExcSlave} {None}
    set_instance_parameter_value cpu {exceptionSlave} {onchip_memory.s1}
    set_instance_parameter_value cpu {breakSlave} {None}
    set_instance_parameter_value cpu {setting_interruptControllerType} {Internal}
    set_instance_parameter_value cpu {setting_branchpredictiontype} {Dynamic}
    set_instance_parameter_value cpu {setting_bhtPtrSz} {8}
    set_instance_parameter_value cpu {cpuArchRev} {1}
    set_instance_parameter_value cpu {mul_shift_choice} {0}
    set_instance_parameter_value cpu {mul_32_impl} {2}
    set_instance_parameter_value cpu {mul_64_impl} {0}
    set_instance_parameter_value cpu {shift_rot_impl} {1}
    set_instance_parameter_value cpu {dividerType} {no_div}
    set_instance_parameter_value cpu {mpu_minInstRegionSize} {12}
    set_instance_parameter_value cpu {mpu_minDataRegionSize} {12}
    set_instance_parameter_value cpu {mmu_uitlbNumEntries} {4}
    set_instance_parameter_value cpu {mmu_udtlbNumEntries} {6}
    set_instance_parameter_value cpu {mmu_tlbPtrSz} {7}
    set_instance_parameter_value cpu {mmu_tlbNumWays} {16}
    set_instance_parameter_value cpu {mmu_processIDNumBits} {8}
    set_instance_parameter_value cpu {impl} {Tiny}
    set_instance_parameter_value cpu {icache_size} {4096}
    set_instance_parameter_value cpu {fa_cache_line} {2}
    set_instance_parameter_value cpu {fa_cache_linesize} {0}
    set_instance_parameter_value cpu {icache_tagramBlockType} {Automatic}
    set_instance_parameter_value cpu {icache_ramBlockType} {Automatic}
    set_instance_parameter_value cpu {icache_numTCIM} {0}
    set_instance_parameter_value cpu {icache_burstType} {None}
    set_instance_parameter_value cpu {dcache_bursts} {false}
    set_instance_parameter_value cpu {dcache_victim_buf_impl} {ram}
    set_instance_parameter_value cpu {dcache_size} {2048}
    set_instance_parameter_value cpu {dcache_tagramBlockType} {Automatic}
    set_instance_parameter_value cpu {dcache_ramBlockType} {Automatic}
    set_instance_parameter_value cpu {dcache_numTCDM} {0}
    set_instance_parameter_value cpu {setting_exportvectors} {0}
    set_instance_parameter_value cpu {setting_usedesignware} {0}
    set_instance_parameter_value cpu {setting_ecc_present} {0}
    set_instance_parameter_value cpu {setting_ic_ecc_present} {1}
    set_instance_parameter_value cpu {setting_rf_ecc_present} {1}
    set_instance_parameter_value cpu {setting_mmu_ecc_present} {1}
    set_instance_parameter_value cpu {setting_dc_ecc_present} {1}
    set_instance_parameter_value cpu {setting_itcm_ecc_present} {1}
    set_instance_parameter_value cpu {setting_dtcm_ecc_present} {1}
    set_instance_parameter_value cpu {regfile_ramBlockType} {Automatic}
    set_instance_parameter_value cpu {ocimem_ramBlockType} {Automatic}
    set_instance_parameter_value cpu {ocimem_ramInit} {0}
    set_instance_parameter_value cpu {mmu_ramBlockType} {Automatic}
    set_instance_parameter_value cpu {bht_ramBlockType} {Automatic}
    set_instance_parameter_value cpu {cdx_enabled} {0}
    set_instance_parameter_value cpu {mpx_enabled} {0}
    set_instance_parameter_value cpu {debug_enabled} {1}
    set_instance_parameter_value cpu {debug_triggerArming} {1}
    set_instance_parameter_value cpu {debug_debugReqSignals} {0}
    set_instance_parameter_value cpu {debug_assignJtagInstanceID} {0}
    set_instance_parameter_value cpu {debug_jtagInstanceID} {0}
    set_instance_parameter_value cpu {debug_OCIOnchipTrace} {_128}
    set_instance_parameter_value cpu {debug_hwbreakpoint} {0}
    set_instance_parameter_value cpu {debug_datatrigger} {0}
    set_instance_parameter_value cpu {debug_traceType} {none}
    set_instance_parameter_value cpu {debug_traceStorage} {onchip_trace}
    set_instance_parameter_value cpu {master_addr_map} {0}
    set_instance_parameter_value cpu {instruction_master_paddr_base} {0}
    set_instance_parameter_value cpu {instruction_master_paddr_size} {0.0}
    set_instance_parameter_value cpu {flash_instruction_master_paddr_base} {0}
    set_instance_parameter_value cpu {flash_instruction_master_paddr_size} {0.0}
    set_instance_parameter_value cpu {data_master_paddr_base} {0}
    set_instance_parameter_value cpu {data_master_paddr_size} {0.0}
    set_instance_parameter_value cpu {tightly_coupled_instruction_master_0_paddr_base} {0}
    set_instance_parameter_value cpu {tightly_coupled_instruction_master_0_paddr_size} {0.0}
    set_instance_parameter_value cpu {tightly_coupled_instruction_master_1_paddr_base} {0}
    set_instance_parameter_value cpu {tightly_coupled_instruction_master_1_paddr_size} {0.0}
    set_instance_parameter_value cpu {tightly_coupled_instruction_master_2_paddr_base} {0}
    set_instance_parameter_value cpu {tightly_coupled_instruction_master_2_paddr_size} {0.0}
    set_instance_parameter_value cpu {tightly_coupled_instruction_master_3_paddr_base} {0}
    set_instance_parameter_value cpu {tightly_coupled_instruction_master_3_paddr_size} {0.0}
    set_instance_parameter_value cpu {tightly_coupled_data_master_0_paddr_base} {0}
    set_instance_parameter_value cpu {tightly_coupled_data_master_0_paddr_size} {0.0}
    set_instance_parameter_value cpu {tightly_coupled_data_master_1_paddr_base} {0}
    set_instance_parameter_value cpu {tightly_coupled_data_master_1_paddr_size} {0.0}
    set_instance_parameter_value cpu {tightly_coupled_data_master_2_paddr_base} {0}
    set_instance_parameter_value cpu {tightly_coupled_data_master_2_paddr_size} {0.0}
    set_instance_parameter_value cpu {tightly_coupled_data_master_3_paddr_base} {0}
    set_instance_parameter_value cpu {tightly_coupled_data_master_3_paddr_size} {0.0}
    set_instance_parameter_value cpu {instruction_master_high_performance_paddr_base} {0}
    set_instance_parameter_value cpu {instruction_master_high_performance_paddr_size} {0.0}
    set_instance_parameter_value cpu {data_master_high_performance_paddr_base} {0}
    set_instance_parameter_value cpu {data_master_high_performance_paddr_size} {0.0}

    add_instance jtag_uart altera_avalon_jtag_uart 15.1
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

    add_instance onchip_memory altera_avalon_onchip_memory2 15.1
    set_instance_parameter_value onchip_memory {allowInSystemMemoryContentEditor} {0}
    set_instance_parameter_value onchip_memory {blockType} {AUTO}
    set_instance_parameter_value onchip_memory {dataWidth} {32}
    set_instance_parameter_value onchip_memory {dualPort} {0}
    set_instance_parameter_value onchip_memory {initMemContent} {0}
    set_instance_parameter_value onchip_memory {initializationFileName} {onchip_mem.hex}
    set_instance_parameter_value onchip_memory {instanceID} {NONE}
    set_instance_parameter_value onchip_memory {memorySize} {131072.0}
    set_instance_parameter_value onchip_memory {readDuringWriteMode} {DONT_CARE}
    set_instance_parameter_value onchip_memory {simAllowMRAMContentsFile} {0}
    set_instance_parameter_value onchip_memory {simMemInitOnlyFilename} {0}
    set_instance_parameter_value onchip_memory {singleClockOperation} {0}
    set_instance_parameter_value onchip_memory {slave1Latency} {1}
    set_instance_parameter_value onchip_memory {slave2Latency} {1}
    set_instance_parameter_value onchip_memory {useNonDefaultInitFile} {0}
    set_instance_parameter_value onchip_memory {copyInitFile} {0}
    set_instance_parameter_value onchip_memory {useShallowMemBlocks} {0}
    set_instance_parameter_value onchip_memory {writable} {1}
    set_instance_parameter_value onchip_memory {ecc_enabled} {0}
    set_instance_parameter_value onchip_memory {resetrequest_enabled} {1}

    add_instance sys_timer altera_avalon_timer 15.1
    set_instance_parameter_value sys_timer {alwaysRun} {0}
    set_instance_parameter_value sys_timer {counterSize} {32}
    set_instance_parameter_value sys_timer {fixedPeriod} {0}
    set_instance_parameter_value sys_timer {period} {1}
    set_instance_parameter_value sys_timer {periodUnits} {MSEC}
    set_instance_parameter_value sys_timer {resetOutput} {0}
    set_instance_parameter_value sys_timer {snapshot} {1}
    set_instance_parameter_value sys_timer {timeoutPulseOutput} {0}
    set_instance_parameter_value sys_timer {watchdogPulse} {2}

    # connections and connection parameters
    add_connection cpu.data_master jtag_uart.avalon_jtag_slave avalon
    set_connection_parameter_value cpu.data_master/jtag_uart.avalon_jtag_slave arbitrationPriority {1}
    set_connection_parameter_value cpu.data_master/jtag_uart.avalon_jtag_slave baseAddress {0x00041020}
    set_connection_parameter_value cpu.data_master/jtag_uart.avalon_jtag_slave defaultConnection {0}

    add_connection cpu.data_master cpu.debug_mem_slave avalon
    set_connection_parameter_value cpu.data_master/cpu.debug_mem_slave arbitrationPriority {1}
    set_connection_parameter_value cpu.data_master/cpu.debug_mem_slave baseAddress {0x00040800}
    set_connection_parameter_value cpu.data_master/cpu.debug_mem_slave defaultConnection {0}

    add_connection cpu.data_master onchip_memory.s1 avalon
    set_connection_parameter_value cpu.data_master/onchip_memory.s1 arbitrationPriority {1}
    set_connection_parameter_value cpu.data_master/onchip_memory.s1 baseAddress {0x00020000}
    set_connection_parameter_value cpu.data_master/onchip_memory.s1 defaultConnection {0}

    add_connection cpu.data_master sys_timer.s1 avalon
    set_connection_parameter_value cpu.data_master/sys_timer.s1 arbitrationPriority {1}
    set_connection_parameter_value cpu.data_master/sys_timer.s1 baseAddress {0x00041000}
    set_connection_parameter_value cpu.data_master/sys_timer.s1 defaultConnection {0}

    add_connection cpu.instruction_master cpu.debug_mem_slave avalon
    set_connection_parameter_value cpu.instruction_master/cpu.debug_mem_slave arbitrationPriority {1}
    set_connection_parameter_value cpu.instruction_master/cpu.debug_mem_slave baseAddress {0x00040800}
    set_connection_parameter_value cpu.instruction_master/cpu.debug_mem_slave defaultConnection {0}

    add_connection cpu.instruction_master onchip_memory.s1 avalon
    set_connection_parameter_value cpu.instruction_master/onchip_memory.s1 arbitrationPriority {1}
    set_connection_parameter_value cpu.instruction_master/onchip_memory.s1 baseAddress {0x00020000}
    set_connection_parameter_value cpu.instruction_master/onchip_memory.s1 defaultConnection {0}

    add_connection clk.clk cpu.clk clock

    add_connection clk.clk jtag_uart.clk clock

    add_connection clk.clk sys_timer.clk clock

    add_connection clk.clk onchip_memory.clk1 clock

    add_connection cpu.irq jtag_uart.irq interrupt
    set_connection_parameter_value cpu.irq/jtag_uart.irq irqNumber {0}

    add_connection cpu.irq sys_timer.irq interrupt
    set_connection_parameter_value cpu.irq/sys_timer.irq irqNumber {1}

    add_connection clk.clk_reset cpu.reset reset

    add_connection clk.clk_reset jtag_uart.reset reset

    add_connection clk.clk_reset sys_timer.reset reset

    add_connection clk.clk_reset onchip_memory.reset1 reset



    # exported interfaces
    add_interface clk clock sink
    set_interface_property clk EXPORT_OF clk.clk_in
    add_interface reset reset sink
    set_interface_property reset EXPORT_OF clk.clk_in_reset

    # interconnect requirements
    set_interconnect_requirement {$system} {qsys_mm.clockCrossingAdapter} {HANDSHAKE}
    set_interconnect_requirement {$system} {qsys_mm.maxAdditionalLatency} {1}
    set_interconnect_requirement {$system} {qsys_mm.enableEccProtection} {FALSE}
    set_interconnect_requirement {$system} {qsys_mm.insertDefaultSlave} {FALSE}

