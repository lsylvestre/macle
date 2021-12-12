library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.misc_gcd.all;

entity gcd is
  port(signal clk : in std_logic;
       signal reset : in std_logic;
       signal a : in caml_int;
       signal b : in caml_int;
       signal start : in std_logic;
       signal rdy : out std_logic;
       signal result : out caml_int);
end entity;
architecture RTL of gcd is
  signal a_0x14 : caml_int := to_signed(0,31);
  signal b_0x15 : caml_int := to_signed(0,31);
  
  type STATE_0x16_T is (GCD_0x13, IDLE);
  signal STATE_0x16 : STATE_0x16_T;
  
begin
  process(reset,clk) begin
    if reset = '1' then
      state_0x16 <= IDLE;
    elsif rising_edge(clk) then
      case STATE_0x16 is
        when GCD_0x13 =>
          if a_0x14 < b_0x15 then
            a_0x14 <= a_0x14;
            b_0x15 <= b_0x15 - a_0x14;
            state_0x16 <= GCD_0x13;
          elsif a_0x14 > b_0x15 then
            a_0x14 <= a_0x14 - b_0x15;
            b_0x15 <= a_0x14;
            state_0x16 <= GCD_0x13;
          else
            result <= a_0x14;
            state_0x16 <= IDLE;
          end if;
        
      when IDLE =>
        if start = '1' then
          rdy <= '0';
          a_0x14 <= a;
          b_0x15 <= b;
          state_0x16 <= GCD_0x13;
        else
          rdy <= '1';
          state_0x16 <= IDLE;
        end if;
      end case;
    end if;
  end process;

end architecture;
