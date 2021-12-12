library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.misc_map_heavy_inc4.all;

entity map_heavy_inc4 is
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
architecture RTL of map_heavy_inc4 is
  signal idx_0xcf : caml_int := to_signed(0,31);
  signal acc_0xea : caml_int := to_signed(0,31);
  signal n_0xeb : caml_int := to_signed(0,31);
  signal x_0xec : caml_int := to_signed(0,31);
  signal ignore_0xed : unit := UNIT_VALUE;
  signal element_0xe8 : caml_int := to_signed(0,31);
  signal acc_0xd9 : caml_int := to_signed(0,31);
  signal n_0xda : caml_int := to_signed(0,31);
  signal start_l_0xfd : std_logic := '-';
  signal rdy_l_0xfd : std_logic := '-';
  signal element_0xdb : caml_int := to_signed(0,31);
  signal acc_0xdd : caml_int := to_signed(0,31);
  signal n_0xde : caml_int := to_signed(0,31);
  signal start_l_0xfe : std_logic := '-';
  signal rdy_l_0xfe : std_logic := '-';
  signal element_0xdf : caml_int := to_signed(0,31);
  signal acc_0xe1 : caml_int := to_signed(0,31);
  signal n_0xe2 : caml_int := to_signed(0,31);
  signal start_l_0xff : std_logic := '-';
  signal rdy_l_0xff : std_logic := '-';
  signal element_0xe3 : caml_int := to_signed(0,31);
  signal acc_0xd5 : caml_int := to_signed(0,31);
  signal n_0xd6 : caml_int := to_signed(0,31);
  signal ignore_0xe7 : unit := UNIT_VALUE;
  signal ignore_0xe6 : unit := UNIT_VALUE;
  signal ignore_0xe5 : unit := UNIT_VALUE;
  signal ignore_0xe4 : unit := UNIT_VALUE;
  signal element_0xd7 : caml_int := to_signed(0,31);
  signal element_0xd3 : caml_int := to_signed(0,31);
  signal element_0xd2 : caml_int := to_signed(0,31);
  signal element_0xd1 : caml_int := to_signed(0,31);
  signal element_0xd0 : caml_int := to_signed(0,31);
  signal size_0xcd : caml_int := to_signed(0,31);
  
  type STATE_0x10b_T is (WAIT_READ_0xEF, WAIT_READ_0xF1, ADD_0xE9,
                         WAIT_WRITE_0xF4, Q_0xF3, Q_0xF2, Q_0xF0,
                         WAIT_READ_0xF6, WAIT_READ_0xF8, WAIT_READ_0xFA,
                         WAIT_READ_0xFC, ADD_0xD4, WAIT_WRITE_0x104,
                         WAIT_WRITE_0x106, WAIT_WRITE_0x108,
                         WAIT_WRITE_0x10A, Q_0x109, Q_0x107, Q_0x105,
                         Q_0x103, Q_0x100, P_0x101, R_0x102, Q_0xFB, Q_0xF9,
                         Q_0xF7, Q_0xF5, AUX_0xCE, Q_0xEE, IDLE);
  signal STATE_0x10b : STATE_0x10b_T;
  
  type STATE_0x10c_T is (ADD_0xD8, IDLE_L_0xFD);
  signal STATE_0x10c : STATE_0x10c_T;
  
  type STATE_0x10d_T is (ADD_0xDC, IDLE_L_0xFE);
  signal STATE_0x10d : STATE_0x10d_T;
  
  type STATE_0x10e_T is (ADD_0xE0, IDLE_L_0xFF);
  signal STATE_0x10e : STATE_0x10e_T;
  
begin
  process(reset,clk) begin
    if reset = '1' then
      state_0x10b <= IDLE;
    elsif rising_edge(clk) then
      case STATE_0x10b is
        when WAIT_READ_0xEF =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            size_0xcd <= size_header(avm_rm_readdata);
            state_0x10b <= Q_0xEE;
          else
            state_0x10b <= WAIT_READ_0xEF;
          end if;
        when WAIT_READ_0xF1 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0xe8 <= signed(avm_rm_readdata(31 downto 1));
            state_0x10b <= Q_0xF0;
          else
            state_0x10b <= WAIT_READ_0xF1;
          end if;
        when ADD_0xE9 =>
          if n_0xeb <= to_signed(0,31) then
            x_0xec <= acc_0xea;
            state_0x10b <= Q_0xF3;
          else
            acc_0xea <= acc_0xea + to_signed(1,31);
            n_0xeb <= n_0xeb - to_signed(1,31);
            state_0x10b <= ADD_0xE9;
          end if;
        when WAIT_WRITE_0xF4 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0xed <= UNIT_VALUE;
            state_0x10b <= Q_0xF2;
          else
            state_0x10b <= WAIT_WRITE_0xF4;
          end if;
        when Q_0xF3 =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0xcf);
          avm_wm_writedata <= std_logic_vector(x_0xec)& "1";
          avm_wm_write <= '1';
          state_0x10b <= WAIT_WRITE_0xF4;
        when Q_0xF2 =>
          idx_0xcf <= idx_0xcf + to_signed(1,31);
          state_0x10b <= AUX_0xCE;
        when Q_0xF0 =>
          acc_0xea <= to_signed(1,31);
          n_0xeb <= element_0xe8;
          state_0x10b <= ADD_0xE9;
        when WAIT_READ_0xF6 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0xd0 <= signed(avm_rm_readdata(31 downto 1));
            state_0x10b <= Q_0xF5;
          else
            state_0x10b <= WAIT_READ_0xF6;
          end if;
        when WAIT_READ_0xF8 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0xd1 <= signed(avm_rm_readdata(31 downto 1));
            state_0x10b <= Q_0xF7;
          else
            state_0x10b <= WAIT_READ_0xF8;
          end if;
        when WAIT_READ_0xFA =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0xd2 <= signed(avm_rm_readdata(31 downto 1));
            state_0x10b <= Q_0xF9;
          else
            state_0x10b <= WAIT_READ_0xFA;
          end if;
        when WAIT_READ_0xFC =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0xd3 <= signed(avm_rm_readdata(31 downto 1));
            state_0x10b <= Q_0xFB;
          else
            state_0x10b <= WAIT_READ_0xFC;
          end if;
        when ADD_0xD4 =>
          if n_0xd6 <= to_signed(0,31) then
            element_0xd7 <= acc_0xd5;
            state_0x10b <= R_0x102;
          else
            acc_0xd5 <= acc_0xd5 + to_signed(1,31);
            n_0xd6 <= n_0xd6 - to_signed(1,31);
            state_0x10b <= ADD_0xD4;
          end if;
        when WAIT_WRITE_0x104 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0xe4 <= UNIT_VALUE;
            state_0x10b <= Q_0x103;
          else
            state_0x10b <= WAIT_WRITE_0x104;
          end if;
        when WAIT_WRITE_0x106 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0xe5 <= UNIT_VALUE;
            state_0x10b <= Q_0x105;
          else
            state_0x10b <= WAIT_WRITE_0x106;
          end if;
        when WAIT_WRITE_0x108 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0xe6 <= UNIT_VALUE;
            state_0x10b <= Q_0x107;
          else
            state_0x10b <= WAIT_WRITE_0x108;
          end if;
        when WAIT_WRITE_0x10A =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0xe7 <= UNIT_VALUE;
            state_0x10b <= Q_0x109;
          else
            state_0x10b <= WAIT_WRITE_0x10A;
          end if;
        when Q_0x109 =>
          idx_0xcf <= idx_0xcf + to_signed(4,31);
          state_0x10b <= AUX_0xCE;
        when Q_0x107 =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0xcf + to_signed(3,31));
          avm_wm_writedata <= std_logic_vector(element_0xe3)& "1";
          avm_wm_write <= '1';
          state_0x10b <= WAIT_WRITE_0x10A;
        when Q_0x105 =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0xcf + to_signed(2,31));
          avm_wm_writedata <= std_logic_vector(element_0xdf)& "1";
          avm_wm_write <= '1';
          state_0x10b <= WAIT_WRITE_0x108;
        when Q_0x103 =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0xcf + to_signed(1,31));
          avm_wm_writedata <= std_logic_vector(element_0xdb)& "1";
          avm_wm_write <= '1';
          state_0x10b <= WAIT_WRITE_0x106;
        when Q_0x100 =>
          start_l_0xfd <= '1';
          start_l_0xfe <= '1';
          start_l_0xff <= '1';
          state_0x10b <= P_0x101;
        when P_0x101 =>
          start_l_0xfd <= '0';
          start_l_0xfe <= '0';
          start_l_0xff <= '0';
          acc_0xd5 <= to_signed(1,31);
          n_0xd6 <= element_0xd0;
          state_0x10b <= ADD_0xD4;
        when R_0x102 =>
          if ((rdy_l_0xfd = '1') and (rdy_l_0xfe = '1')) and (rdy_l_0xff = '1') then
            avm_wm_address <= compute_address(caml_heap_base, a, idx_0xcf + to_signed(0,31));
            avm_wm_writedata <= std_logic_vector(element_0xd7)& "1";
            avm_wm_write <= '1';
            state_0x10b <= WAIT_WRITE_0x104;
          else
            state_0x10b <= R_0x102;
          end if;
        when Q_0xFB =>
          state_0x10b <= Q_0x100;
        when Q_0xF9 =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0xcf + to_signed(3,31));
          avm_rm_read <= '1';
          state_0x10b <= WAIT_READ_0xFC;
        when Q_0xF7 =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0xcf + to_signed(2,31));
          avm_rm_read <= '1';
          state_0x10b <= WAIT_READ_0xFA;
        when Q_0xF5 =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0xcf + to_signed(1,31));
          avm_rm_read <= '1';
          state_0x10b <= WAIT_READ_0xF8;
        when AUX_0xCE =>
          if idx_0xcf >= size_0xcd then
            result <= UNIT_VALUE;
            state_0x10b <= IDLE;
          elsif idx_0xcf > (size_0xcd - to_signed(4,31)) then
            avm_rm_address <= compute_address(caml_heap_base, a, idx_0xcf);
            avm_rm_read <= '1';
            state_0x10b <= WAIT_READ_0xF1;
          else
            avm_rm_address <= compute_address(caml_heap_base, a, idx_0xcf + to_signed(0,31));
            avm_rm_read <= '1';
            state_0x10b <= WAIT_READ_0xF6;
          end if;
        
      when Q_0xEE =>
        idx_0xcf <= to_signed(0,31);
        state_0x10b <= AUX_0xCE;
      when IDLE =>
        if start = '1' then
          rdy <= '0';
          avm_rm_address <= compute_address(caml_heap_base, a, to_signed(-1,31));
          avm_rm_read <= '1';
          state_0x10b <= WAIT_READ_0xEF;
        else
          rdy <= '1';
          state_0x10b <= IDLE;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x10c <= IDLE_L_0xFD;
  elsif rising_edge(clk) then
    case STATE_0x10c is
      when ADD_0xD8 =>
        if n_0xda <= to_signed(0,31) then
          element_0xdb <= acc_0xd9;
          state_0x10c <= IDLE_L_0xFD;
        else
          acc_0xd9 <= acc_0xd9 + to_signed(1,31);
          n_0xda <= n_0xda - to_signed(1,31);
          state_0x10c <= ADD_0xD8;
        end if;
      when IDLE_L_0xFD =>
        if start_l_0xfd = '1' then
          rdy_l_0xfd <= '0';
          acc_0xd9 <= to_signed(1,31);
          n_0xda <= element_0xd1;
          state_0x10c <= ADD_0xD8;
        else
          rdy_l_0xfd <= '1';
          state_0x10c <= IDLE_L_0xFD;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x10d <= IDLE_L_0xFE;
  elsif rising_edge(clk) then
    case STATE_0x10d is
      when ADD_0xDC =>
        if n_0xde <= to_signed(0,31) then
          element_0xdf <= acc_0xdd;
          state_0x10d <= IDLE_L_0xFE;
        else
          acc_0xdd <= acc_0xdd + to_signed(1,31);
          n_0xde <= n_0xde - to_signed(1,31);
          state_0x10d <= ADD_0xDC;
        end if;
      when IDLE_L_0xFE =>
        if start_l_0xfe = '1' then
          rdy_l_0xfe <= '0';
          acc_0xdd <= to_signed(1,31);
          n_0xde <= element_0xd2;
          state_0x10d <= ADD_0xDC;
        else
          rdy_l_0xfe <= '1';
          state_0x10d <= IDLE_L_0xFE;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x10e <= IDLE_L_0xFF;
  elsif rising_edge(clk) then
    case STATE_0x10e is
      when ADD_0xE0 =>
        if n_0xe2 <= to_signed(0,31) then
          element_0xe3 <= acc_0xe1;
          state_0x10e <= IDLE_L_0xFF;
        else
          acc_0xe1 <= acc_0xe1 + to_signed(1,31);
          n_0xe2 <= n_0xe2 - to_signed(1,31);
          state_0x10e <= ADD_0xE0;
        end if;
      when IDLE_L_0xFF =>
        if start_l_0xff = '1' then
          rdy_l_0xff <= '0';
          acc_0xe1 <= to_signed(1,31);
          n_0xe2 <= element_0xd3;
          state_0x10e <= ADD_0xE0;
        else
          rdy_l_0xff <= '1';
          state_0x10e <= IDLE_L_0xFF;
        end if;
      end case;
    end if;
  end process;

end architecture;
