add_instance map_heavy_inc2_cc MAP_HEAVY_INC2_CC 1.0

add_connection cpu.data_master map_heavy_inc2_cc.s0 avalon
set_connection_parameter_value cpu.data_master/map_heavy_inc2_cc.s0 arbitrationPriority {1}
set_connection_parameter_value cpu.data_master/map_heavy_inc2_cc.s0 baseAddress {0x10001000}
set_connection_parameter_value cpu.data_master/map_heavy_inc2_cc.s0 defaultConnection {0}
add_connection map_heavy_inc2_cc.rm onchip_memory.s1 avalon
set_connection_parameter_value map_heavy_inc2_cc.rm/onchip_memory.s1 arbitrationPriority {1}
set_connection_parameter_value map_heavy_inc2_cc.rm/onchip_memory.s1 baseAddress {0x00020000}
set_connection_parameter_value map_heavy_inc2_cc.rm/onchip_memory.s1 defaultConnection {0}
add_connection map_heavy_inc2_cc.wm onchip_memory.s1 avalon
set_connection_parameter_value map_heavy_inc2_cc.wm/onchip_memory.s1 arbitrationPriority {1}
set_connection_parameter_value map_heavy_inc2_cc.wm/onchip_memory.s1 baseAddress {0x00020000}
set_connection_parameter_value map_heavy_inc2_cc.wm/onchip_memory.s1 defaultConnection {0}
add_connection clk.clk map_heavy_inc2_cc.clock clock
add_connection clk.clk_reset map_heavy_inc2_cc.reset reset

