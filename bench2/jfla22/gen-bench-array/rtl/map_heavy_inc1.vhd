library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.misc_map_heavy_inc1.all;

entity map_heavy_inc1 is
  port(signal clk : in std_logic;
       signal reset : in std_logic;
       signal a : in caml_value;
       signal start : in std_logic;
       signal avm_wm_waitrequest : in std_logic;
       signal avm_rm_waitrequest : in std_logic;
       signal avm_rm_readdata : in caml_value;
       signal caml_heap_base : in caml_value;
       signal rdy : out std_logic;
       signal result : out unit;
       signal avm_wm_writedata : out caml_value;
       signal avm_wm_address : out caml_value;
       signal avm_wm_write : out std_logic;
       signal avm_rm_address : out caml_value;
       signal avm_rm_read : out std_logic);
end entity;
architecture RTL of map_heavy_inc1 is
  signal idx_0x25 : caml_int := to_signed(0,31);
  signal acc_0x2e : caml_int := to_signed(0,31);
  signal n_0x2f : caml_int := to_signed(0,31);
  signal x_0x30 : caml_int := to_signed(0,31);
  signal ignore_0x31 : unit := UNIT_VALUE;
  signal element_0x2c : caml_int := to_signed(0,31);
  signal acc_0x28 : caml_int := to_signed(0,31);
  signal n_0x29 : caml_int := to_signed(0,31);
  signal ignore_0x2b : unit := UNIT_VALUE;
  signal element_0x2a : caml_int := to_signed(0,31);
  signal element_0x26 : caml_int := to_signed(0,31);
  signal size_0x23 : caml_int := to_signed(0,31);
  
  type STATE_0x3e_T is (WAIT_READ_0x33, WAIT_READ_0x35, ADD_0x2D,
                        WAIT_WRITE_0x38, Q_0x37, Q_0x36, Q_0x34,
                        WAIT_READ_0x3A, ADD_0x27, WAIT_WRITE_0x3D, Q_0x3C,
                        Q_0x3B, Q_0x39, AUX_0x24, Q_0x32, IDLE);
  signal STATE_0x3e : STATE_0x3e_T;
  
begin
  process(reset,clk) begin
    if reset = '1' then
      state_0x3e <= IDLE;
    elsif rising_edge(clk) then
      case STATE_0x3e is
        when WAIT_READ_0x33 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            size_0x23 <= size_header(avm_rm_readdata);
            state_0x3e <= Q_0x32;
          else
            state_0x3e <= WAIT_READ_0x33;
          end if;
        when WAIT_READ_0x35 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x2c <= signed(avm_rm_readdata(31 downto 1));
            state_0x3e <= Q_0x34;
          else
            state_0x3e <= WAIT_READ_0x35;
          end if;
        when ADD_0x2D =>
          if n_0x2f <= to_signed(0,31) then
            x_0x30 <= acc_0x2e;
            state_0x3e <= Q_0x37;
          else
            acc_0x2e <= acc_0x2e + to_signed(1,31);
            n_0x2f <= n_0x2f - to_signed(1,31);
            state_0x3e <= ADD_0x2D;
          end if;
        when WAIT_WRITE_0x38 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x31 <= UNIT_VALUE;
            state_0x3e <= Q_0x36;
          else
            state_0x3e <= WAIT_WRITE_0x38;
          end if;
        when Q_0x37 =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x25);
          avm_wm_writedata <= std_logic_vector(x_0x30)& "1";
          avm_wm_write <= '1';
          state_0x3e <= WAIT_WRITE_0x38;
        when Q_0x36 =>
          idx_0x25 <= idx_0x25 + to_signed(1,31);
          state_0x3e <= AUX_0x24;
        when Q_0x34 =>
          acc_0x2e <= to_signed(1,31);
          n_0x2f <= element_0x2c;
          state_0x3e <= ADD_0x2D;
        when WAIT_READ_0x3A =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x26 <= signed(avm_rm_readdata(31 downto 1));
            state_0x3e <= Q_0x39;
          else
            state_0x3e <= WAIT_READ_0x3A;
          end if;
        when ADD_0x27 =>
          if n_0x29 <= to_signed(0,31) then
            element_0x2a <= acc_0x28;
            state_0x3e <= Q_0x3B;
          else
            acc_0x28 <= acc_0x28 + to_signed(1,31);
            n_0x29 <= n_0x29 - to_signed(1,31);
            state_0x3e <= ADD_0x27;
          end if;
        when WAIT_WRITE_0x3D =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x2b <= UNIT_VALUE;
            state_0x3e <= Q_0x3C;
          else
            state_0x3e <= WAIT_WRITE_0x3D;
          end if;
        when Q_0x3C =>
          idx_0x25 <= idx_0x25 + to_signed(1,31);
          state_0x3e <= AUX_0x24;
        when Q_0x3B =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x25 + to_signed(0,31));
          avm_wm_writedata <= std_logic_vector(element_0x2a)& "1";
          avm_wm_write <= '1';
          state_0x3e <= WAIT_WRITE_0x3D;
        when Q_0x39 =>
          acc_0x28 <= to_signed(1,31);
          n_0x29 <= element_0x26;
          state_0x3e <= ADD_0x27;
        when AUX_0x24 =>
          if idx_0x25 >= size_0x23 then
            result <= UNIT_VALUE;
            state_0x3e <= IDLE;
          elsif idx_0x25 > (size_0x23 - to_signed(1,31)) then
            avm_rm_address <= compute_address(caml_heap_base, a, idx_0x25);
            avm_rm_read <= '1';
            state_0x3e <= WAIT_READ_0x35;
          else
            avm_rm_address <= compute_address(caml_heap_base, a, idx_0x25 + to_signed(0,31));
            avm_rm_read <= '1';
            state_0x3e <= WAIT_READ_0x3A;
          end if;
        
      when Q_0x32 =>
        idx_0x25 <= to_signed(0,31);
        state_0x3e <= AUX_0x24;
      when IDLE =>
        if start = '1' then
          rdy <= '0';
          avm_rm_address <= compute_address(caml_heap_base, a, to_signed(-1,31));
          avm_rm_read <= '1';
          state_0x3e <= WAIT_READ_0x33;
        else
          rdy <= '1';
          state_0x3e <= IDLE;
        end if;
      end case;
    end if;
  end process;

end architecture;
