-- AVALON MM-slave wrapper around the core gcd IP
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.misc_gcd.all;

entity avs_gcd is
  port (avs_s0_address : in std_logic_vector(3 downto 0)  := (others => '0');
          -- 0000  : control/status register (b1=start, b0=rdy)
          -- 0001  : a register
          -- 0010  : b register
          -- 0011  : result register
          
        avs_s0_read        : in  std_logic                     := '0';
        avs_s0_readdata    : out std_logic_vector(31 downto 0);
        avs_s0_write       : in  std_logic                     := '0';
        avs_s0_writedata   : in  std_logic_vector(31 downto 0) := (others => '0');
        clock_clk          : in  std_logic                     := '0';
        reset_reset        : in  std_logic                     := '0');
end entity;

architecture rtl of avs_gcd is
  component gcd is
    port (signal clk : in std_logic;
          signal reset : in std_logic;
          signal start : in std_logic;
          signal rdy : out std_logic;
          signal a: in caml_int;
          signal b: in caml_int;
          signal result: out caml_int);
  end component;
  
  signal a: caml_int;
  signal b: caml_int;
  signal result: caml_int;
  signal start: std_logic;
signal rdy: std_logic;type write_state_t is (Idle, StartAsserted);
signal write_state: write_state_t;
begin
  gcd_CC : component gcd
    port map (clk => clock_clk,
              reset => reset_reset,
              start => start,
              rdy => rdy,
              a => a,
              b => b,
              result => result);
  
  WRITE: process (clock_clk, reset_reset)
  begin
    if reset_reset = '1' then
      write_state <= Idle;
    elsif rising_edge(clock_clk) then
      case write_state is
        when StartAsserted =>
          start <= '0';
          write_state <= Idle;
        when Idle =>
          if avs_s0_write = '1' then
            case avs_s0_address is
              when "0000" => -- writing CSR asserts start  for one clock period
                start <= '1';
                write_state <= StartAsserted;
              when "0001" => a <= signed(avs_s0_writedata(30 downto 0));
              when "0010" => b <= signed(avs_s0_writedata(30 downto 0));
              when others => NULL;
            end case;
          end if;
        end case;
      end if;
    end process;
  READ: process (clock_clk)
  begin
    if rising_edge(clock_clk) then
      if avs_s0_read = '1' then
        case avs_s0_address is
          when "0000" => avs_s0_readdata <= X"0000000" & "000" & rdy;
                         -- when reading CSR, bit 0 is rdy
          when "0001" => avs_s0_readdata <= "0" & std_logic_vector(a);
          when "0010" => avs_s0_readdata <= "0" & std_logic_vector(b);
          when "0011" => avs_s0_readdata <= "0" & std_logic_vector(result);
          when others => null;
          end case;
        end if;
      end if;
    end process;
  end architecture;
