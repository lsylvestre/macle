library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.misc_map_heavy_inc2.all;

entity map_heavy_inc2 is
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
architecture RTL of map_heavy_inc2 is
  signal idx_0x6b : caml_int := to_signed(0,31);
  signal acc_0x7a : caml_int := to_signed(0,31);
  signal n_0x7b : caml_int := to_signed(0,31);
  signal x_0x7c : caml_int := to_signed(0,31);
  signal ignore_0x7d : unit := UNIT_VALUE;
  signal element_0x78 : caml_int := to_signed(0,31);
  signal acc_0x73 : caml_int := to_signed(0,31);
  signal n_0x74 : caml_int := to_signed(0,31);
  signal start_l_0x89 : std_logic := '-';
  signal rdy_l_0x89 : std_logic := '-';
  signal element_0x75 : caml_int := to_signed(0,31);
  signal acc_0x6f : caml_int := to_signed(0,31);
  signal n_0x70 : caml_int := to_signed(0,31);
  signal ignore_0x77 : unit := UNIT_VALUE;
  signal ignore_0x76 : unit := UNIT_VALUE;
  signal element_0x71 : caml_int := to_signed(0,31);
  signal element_0x6d : caml_int := to_signed(0,31);
  signal element_0x6c : caml_int := to_signed(0,31);
  signal size_0x69 : caml_int := to_signed(0,31);
  
  type STATE_0x91_T is (WAIT_READ_0x7F, WAIT_READ_0x81, ADD_0x79,
                        WAIT_WRITE_0x84, Q_0x83, Q_0x82, Q_0x80,
                        WAIT_READ_0x86, WAIT_READ_0x88, ADD_0x6E,
                        WAIT_WRITE_0x8E, WAIT_WRITE_0x90, Q_0x8F, Q_0x8D,
                        Q_0x8A, P_0x8B, R_0x8C, Q_0x87, Q_0x85, AUX_0x6A,
                        Q_0x7E, IDLE);
  signal STATE_0x91 : STATE_0x91_T;
  
  type STATE_0x92_T is (ADD_0x72, IDLE_L_0x89);
  signal STATE_0x92 : STATE_0x92_T;
  
begin
  process(reset,clk) begin
    if reset = '1' then
      state_0x91 <= IDLE;
    elsif rising_edge(clk) then
      case STATE_0x91 is
        when WAIT_READ_0x7F =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            size_0x69 <= size_header(avm_rm_readdata);
            state_0x91 <= Q_0x7E;
          else
            state_0x91 <= WAIT_READ_0x7F;
          end if;
        when WAIT_READ_0x81 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x78 <= signed(avm_rm_readdata(31 downto 1));
            state_0x91 <= Q_0x80;
          else
            state_0x91 <= WAIT_READ_0x81;
          end if;
        when ADD_0x79 =>
          if n_0x7b <= to_signed(0,31) then
            x_0x7c <= acc_0x7a;
            state_0x91 <= Q_0x83;
          else
            acc_0x7a <= acc_0x7a + to_signed(1,31);
            n_0x7b <= n_0x7b - to_signed(1,31);
            state_0x91 <= ADD_0x79;
          end if;
        when WAIT_WRITE_0x84 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x7d <= UNIT_VALUE;
            state_0x91 <= Q_0x82;
          else
            state_0x91 <= WAIT_WRITE_0x84;
          end if;
        when Q_0x83 =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x6b);
          avm_wm_writedata <= std_logic_vector(x_0x7c)& "1";
          avm_wm_write <= '1';
          state_0x91 <= WAIT_WRITE_0x84;
        when Q_0x82 =>
          idx_0x6b <= idx_0x6b + to_signed(1,31);
          state_0x91 <= AUX_0x6A;
        when Q_0x80 =>
          acc_0x7a <= to_signed(1,31);
          n_0x7b <= element_0x78;
          state_0x91 <= ADD_0x79;
        when WAIT_READ_0x86 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x6c <= signed(avm_rm_readdata(31 downto 1));
            state_0x91 <= Q_0x85;
          else
            state_0x91 <= WAIT_READ_0x86;
          end if;
        when WAIT_READ_0x88 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x6d <= signed(avm_rm_readdata(31 downto 1));
            state_0x91 <= Q_0x87;
          else
            state_0x91 <= WAIT_READ_0x88;
          end if;
        when ADD_0x6E =>
          if n_0x70 <= to_signed(0,31) then
            element_0x71 <= acc_0x6f;
            state_0x91 <= R_0x8C;
          else
            acc_0x6f <= acc_0x6f + to_signed(1,31);
            n_0x70 <= n_0x70 - to_signed(1,31);
            state_0x91 <= ADD_0x6E;
          end if;
        when WAIT_WRITE_0x8E =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x76 <= UNIT_VALUE;
            state_0x91 <= Q_0x8D;
          else
            state_0x91 <= WAIT_WRITE_0x8E;
          end if;
        when WAIT_WRITE_0x90 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x77 <= UNIT_VALUE;
            state_0x91 <= Q_0x8F;
          else
            state_0x91 <= WAIT_WRITE_0x90;
          end if;
        when Q_0x8F =>
          idx_0x6b <= idx_0x6b + to_signed(2,31);
          state_0x91 <= AUX_0x6A;
        when Q_0x8D =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x6b + to_signed(1,31));
          avm_wm_writedata <= std_logic_vector(element_0x75)& "1";
          avm_wm_write <= '1';
          state_0x91 <= WAIT_WRITE_0x90;
        when Q_0x8A =>
          start_l_0x89 <= '1';
          state_0x91 <= P_0x8B;
        when P_0x8B =>
          start_l_0x89 <= '0';
          acc_0x6f <= to_signed(1,31);
          n_0x70 <= element_0x6c;
          state_0x91 <= ADD_0x6E;
        when R_0x8C =>
          if rdy_l_0x89 = '1' then
            avm_wm_address <= compute_address(caml_heap_base, a, idx_0x6b + to_signed(0,31));
            avm_wm_writedata <= std_logic_vector(element_0x71)& "1";
            avm_wm_write <= '1';
            state_0x91 <= WAIT_WRITE_0x8E;
          else
            state_0x91 <= R_0x8C;
          end if;
        when Q_0x87 =>
          state_0x91 <= Q_0x8A;
        when Q_0x85 =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x6b + to_signed(1,31));
          avm_rm_read <= '1';
          state_0x91 <= WAIT_READ_0x88;
        when AUX_0x6A =>
          if idx_0x6b >= size_0x69 then
            result <= UNIT_VALUE;
            state_0x91 <= IDLE;
          elsif idx_0x6b > (size_0x69 - to_signed(2,31)) then
            avm_rm_address <= compute_address(caml_heap_base, a, idx_0x6b);
            avm_rm_read <= '1';
            state_0x91 <= WAIT_READ_0x81;
          else
            avm_rm_address <= compute_address(caml_heap_base, a, idx_0x6b + to_signed(0,31));
            avm_rm_read <= '1';
            state_0x91 <= WAIT_READ_0x86;
          end if;
        
      when Q_0x7E =>
        idx_0x6b <= to_signed(0,31);
        state_0x91 <= AUX_0x6A;
      when IDLE =>
        if start = '1' then
          rdy <= '0';
          avm_rm_address <= compute_address(caml_heap_base, a, to_signed(-1,31));
          avm_rm_read <= '1';
          state_0x91 <= WAIT_READ_0x7F;
        else
          rdy <= '1';
          state_0x91 <= IDLE;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x92 <= IDLE_L_0x89;
  elsif rising_edge(clk) then
    case STATE_0x92 is
      when ADD_0x72 =>
        if n_0x74 <= to_signed(0,31) then
          element_0x75 <= acc_0x73;
          state_0x92 <= IDLE_L_0x89;
        else
          acc_0x73 <= acc_0x73 + to_signed(1,31);
          n_0x74 <= n_0x74 - to_signed(1,31);
          state_0x92 <= ADD_0x72;
        end if;
      when IDLE_L_0x89 =>
        if start_l_0x89 = '1' then
          rdy_l_0x89 <= '0';
          acc_0x73 <= to_signed(1,31);
          n_0x74 <= element_0x6d;
          state_0x92 <= ADD_0x72;
        else
          rdy_l_0x89 <= '1';
          state_0x92 <= IDLE_L_0x89;
        end if;
      end case;
    end if;
  end process;

end architecture;
