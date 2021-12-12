add_instance fact_cc FACT_CC 1.0

add_connection cpu.data_master fact_cc.s0 avalon
set_connection_parameter_value cpu.data_master/fact_cc.s0 arbitrationPriority {1}
set_connection_parameter_value cpu.data_master/fact_cc.s0 baseAddress {0x10000000}
set_connection_parameter_value cpu.data_master/fact_cc.s0 defaultConnection {0}
add_connection clk.clk fact_cc.clock clock
add_connection clk.clk_reset fact_cc.reset reset

