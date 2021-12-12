library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.misc_fact.all;

entity fact is
  port(signal clk : in std_logic;
       signal reset : in std_logic;
       signal n : in caml_int;
       signal start : in std_logic;
       signal rdy : out std_logic;
       signal result : out caml_int);
end entity;
architecture RTL of fact is
  signal acc_0x9 : caml_int := to_signed(0,31);
  signal n_0xa : caml_int := to_signed(0,31);
  
  type STATE_0xb_T is (AUX_0x8, IDLE);
  signal STATE_0xb : STATE_0xb_T;
  
begin
  process(reset,clk) begin
    if reset = '1' then
      state_0xb <= IDLE;
    elsif rising_edge(clk) then
      case STATE_0xb is
        when AUX_0x8 =>
          if n_0xa <= to_signed(0,31) then
            result <= acc_0x9;
            state_0xb <= IDLE;
          else
            acc_0x9 <= RESIZE((acc_0x9 * n_0xa),31);
            n_0xa <= n_0xa - to_signed(1,31);
            state_0xb <= AUX_0x8;
          end if;
        when IDLE =>
          if start = '1' then
            rdy <= '0';
            acc_0x9 <= to_signed(1,31);
            n_0xa <= n;
            state_0xb <= AUX_0x8;
          else
            rdy <= '1';
            state_0xb <= IDLE;
          end if;
        end case;
      end if;
    end process;
  
end architecture;
