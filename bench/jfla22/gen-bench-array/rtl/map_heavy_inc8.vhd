library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.misc_map_heavy_inc8.all;

entity map_heavy_inc8 is
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
architecture RTL of map_heavy_inc8 is
  signal idx_0x16b : caml_int := to_signed(0,31);
  signal acc_0x19e : caml_int := to_signed(0,31);
  signal n_0x19f : caml_int := to_signed(0,31);
  signal x_0x1a0 : caml_int := to_signed(0,31);
  signal ignore_0x1a1 : unit := UNIT_VALUE;
  signal element_0x19c : caml_int := to_signed(0,31);
  signal acc_0x179 : caml_int := to_signed(0,31);
  signal n_0x17a : caml_int := to_signed(0,31);
  signal start_l_0x1b9 : std_logic := '-';
  signal rdy_l_0x1b9 : std_logic := '-';
  signal element_0x17b : caml_int := to_signed(0,31);
  signal acc_0x17d : caml_int := to_signed(0,31);
  signal n_0x17e : caml_int := to_signed(0,31);
  signal start_l_0x1ba : std_logic := '-';
  signal rdy_l_0x1ba : std_logic := '-';
  signal element_0x17f : caml_int := to_signed(0,31);
  signal acc_0x181 : caml_int := to_signed(0,31);
  signal n_0x182 : caml_int := to_signed(0,31);
  signal start_l_0x1bb : std_logic := '-';
  signal rdy_l_0x1bb : std_logic := '-';
  signal element_0x183 : caml_int := to_signed(0,31);
  signal acc_0x185 : caml_int := to_signed(0,31);
  signal n_0x186 : caml_int := to_signed(0,31);
  signal start_l_0x1bc : std_logic := '-';
  signal rdy_l_0x1bc : std_logic := '-';
  signal element_0x187 : caml_int := to_signed(0,31);
  signal acc_0x189 : caml_int := to_signed(0,31);
  signal n_0x18a : caml_int := to_signed(0,31);
  signal start_l_0x1bd : std_logic := '-';
  signal rdy_l_0x1bd : std_logic := '-';
  signal element_0x18b : caml_int := to_signed(0,31);
  signal acc_0x18d : caml_int := to_signed(0,31);
  signal n_0x18e : caml_int := to_signed(0,31);
  signal start_l_0x1be : std_logic := '-';
  signal rdy_l_0x1be : std_logic := '-';
  signal element_0x18f : caml_int := to_signed(0,31);
  signal acc_0x191 : caml_int := to_signed(0,31);
  signal n_0x192 : caml_int := to_signed(0,31);
  signal start_l_0x1bf : std_logic := '-';
  signal rdy_l_0x1bf : std_logic := '-';
  signal element_0x193 : caml_int := to_signed(0,31);
  signal acc_0x175 : caml_int := to_signed(0,31);
  signal n_0x176 : caml_int := to_signed(0,31);
  signal ignore_0x19b : unit := UNIT_VALUE;
  signal ignore_0x19a : unit := UNIT_VALUE;
  signal ignore_0x199 : unit := UNIT_VALUE;
  signal ignore_0x198 : unit := UNIT_VALUE;
  signal ignore_0x197 : unit := UNIT_VALUE;
  signal ignore_0x196 : unit := UNIT_VALUE;
  signal ignore_0x195 : unit := UNIT_VALUE;
  signal ignore_0x194 : unit := UNIT_VALUE;
  signal element_0x177 : caml_int := to_signed(0,31);
  signal element_0x173 : caml_int := to_signed(0,31);
  signal element_0x172 : caml_int := to_signed(0,31);
  signal element_0x171 : caml_int := to_signed(0,31);
  signal element_0x170 : caml_int := to_signed(0,31);
  signal element_0x16f : caml_int := to_signed(0,31);
  signal element_0x16e : caml_int := to_signed(0,31);
  signal element_0x16d : caml_int := to_signed(0,31);
  signal element_0x16c : caml_int := to_signed(0,31);
  signal size_0x169 : caml_int := to_signed(0,31);
  
  type STATE_0x1d3_T is (WAIT_READ_0x1A3, WAIT_READ_0x1A5, ADD_0x19D,
                         WAIT_WRITE_0x1A8, Q_0x1A7, Q_0x1A6, Q_0x1A4,
                         WAIT_READ_0x1AA, WAIT_READ_0x1AC, WAIT_READ_0x1AE,
                         WAIT_READ_0x1B0, WAIT_READ_0x1B2, WAIT_READ_0x1B4,
                         WAIT_READ_0x1B6, WAIT_READ_0x1B8, ADD_0x174,
                         WAIT_WRITE_0x1C4, WAIT_WRITE_0x1C6,
                         WAIT_WRITE_0x1C8, WAIT_WRITE_0x1CA,
                         WAIT_WRITE_0x1CC, WAIT_WRITE_0x1CE,
                         WAIT_WRITE_0x1D0, WAIT_WRITE_0x1D2, Q_0x1D1,
                         Q_0x1CF, Q_0x1CD, Q_0x1CB, Q_0x1C9, Q_0x1C7,
                         Q_0x1C5, Q_0x1C3, Q_0x1C0, P_0x1C1, R_0x1C2,
                         Q_0x1B7, Q_0x1B5, Q_0x1B3, Q_0x1B1, Q_0x1AF,
                         Q_0x1AD, Q_0x1AB, Q_0x1A9, AUX_0x16A, Q_0x1A2,
                         IDLE);
  signal STATE_0x1d3 : STATE_0x1d3_T;
  
  type STATE_0x1d4_T is (ADD_0x178, IDLE_L_0x1B9);
  signal STATE_0x1d4 : STATE_0x1d4_T;
  
  type STATE_0x1d5_T is (ADD_0x17C, IDLE_L_0x1BA);
  signal STATE_0x1d5 : STATE_0x1d5_T;
  
  type STATE_0x1d6_T is (ADD_0x180, IDLE_L_0x1BB);
  signal STATE_0x1d6 : STATE_0x1d6_T;
  
  type STATE_0x1d7_T is (ADD_0x184, IDLE_L_0x1BC);
  signal STATE_0x1d7 : STATE_0x1d7_T;
  
  type STATE_0x1d8_T is (ADD_0x188, IDLE_L_0x1BD);
  signal STATE_0x1d8 : STATE_0x1d8_T;
  
  type STATE_0x1d9_T is (ADD_0x18C, IDLE_L_0x1BE);
  signal STATE_0x1d9 : STATE_0x1d9_T;
  
  type STATE_0x1da_T is (ADD_0x190, IDLE_L_0x1BF);
  signal STATE_0x1da : STATE_0x1da_T;
  
begin
  process(reset,clk) begin
    if reset = '1' then
      state_0x1d3 <= IDLE;
    elsif rising_edge(clk) then
      case STATE_0x1d3 is
        when WAIT_READ_0x1A3 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            size_0x169 <= size_header(avm_rm_readdata);
            state_0x1d3 <= Q_0x1A2;
          else
            state_0x1d3 <= WAIT_READ_0x1A3;
          end if;
        when WAIT_READ_0x1A5 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x19c <= signed(avm_rm_readdata(31 downto 1));
            state_0x1d3 <= Q_0x1A4;
          else
            state_0x1d3 <= WAIT_READ_0x1A5;
          end if;
        when ADD_0x19D =>
          if n_0x19f <= to_signed(0,31) then
            x_0x1a0 <= acc_0x19e;
            state_0x1d3 <= Q_0x1A7;
          else
            acc_0x19e <= acc_0x19e + to_signed(1,31);
            n_0x19f <= n_0x19f - to_signed(1,31);
            state_0x1d3 <= ADD_0x19D;
          end if;
        when WAIT_WRITE_0x1A8 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x1a1 <= UNIT_VALUE;
            state_0x1d3 <= Q_0x1A6;
          else
            state_0x1d3 <= WAIT_WRITE_0x1A8;
          end if;
        when Q_0x1A7 =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x16b);
          avm_wm_writedata <= std_logic_vector(x_0x1a0)& "1";
          avm_wm_write <= '1';
          state_0x1d3 <= WAIT_WRITE_0x1A8;
        when Q_0x1A6 =>
          idx_0x16b <= idx_0x16b + to_signed(1,31);
          state_0x1d3 <= AUX_0x16A;
        when Q_0x1A4 =>
          acc_0x19e <= to_signed(1,31);
          n_0x19f <= element_0x19c;
          state_0x1d3 <= ADD_0x19D;
        when WAIT_READ_0x1AA =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x16c <= signed(avm_rm_readdata(31 downto 1));
            state_0x1d3 <= Q_0x1A9;
          else
            state_0x1d3 <= WAIT_READ_0x1AA;
          end if;
        when WAIT_READ_0x1AC =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x16d <= signed(avm_rm_readdata(31 downto 1));
            state_0x1d3 <= Q_0x1AB;
          else
            state_0x1d3 <= WAIT_READ_0x1AC;
          end if;
        when WAIT_READ_0x1AE =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x16e <= signed(avm_rm_readdata(31 downto 1));
            state_0x1d3 <= Q_0x1AD;
          else
            state_0x1d3 <= WAIT_READ_0x1AE;
          end if;
        when WAIT_READ_0x1B0 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x16f <= signed(avm_rm_readdata(31 downto 1));
            state_0x1d3 <= Q_0x1AF;
          else
            state_0x1d3 <= WAIT_READ_0x1B0;
          end if;
        when WAIT_READ_0x1B2 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x170 <= signed(avm_rm_readdata(31 downto 1));
            state_0x1d3 <= Q_0x1B1;
          else
            state_0x1d3 <= WAIT_READ_0x1B2;
          end if;
        when WAIT_READ_0x1B4 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x171 <= signed(avm_rm_readdata(31 downto 1));
            state_0x1d3 <= Q_0x1B3;
          else
            state_0x1d3 <= WAIT_READ_0x1B4;
          end if;
        when WAIT_READ_0x1B6 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x172 <= signed(avm_rm_readdata(31 downto 1));
            state_0x1d3 <= Q_0x1B5;
          else
            state_0x1d3 <= WAIT_READ_0x1B6;
          end if;
        when WAIT_READ_0x1B8 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x173 <= signed(avm_rm_readdata(31 downto 1));
            state_0x1d3 <= Q_0x1B7;
          else
            state_0x1d3 <= WAIT_READ_0x1B8;
          end if;
        when ADD_0x174 =>
          if n_0x176 <= to_signed(0,31) then
            element_0x177 <= acc_0x175;
            state_0x1d3 <= R_0x1C2;
          else
            acc_0x175 <= acc_0x175 + to_signed(1,31);
            n_0x176 <= n_0x176 - to_signed(1,31);
            state_0x1d3 <= ADD_0x174;
          end if;
        when WAIT_WRITE_0x1C4 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x194 <= UNIT_VALUE;
            state_0x1d3 <= Q_0x1C3;
          else
            state_0x1d3 <= WAIT_WRITE_0x1C4;
          end if;
        when WAIT_WRITE_0x1C6 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x195 <= UNIT_VALUE;
            state_0x1d3 <= Q_0x1C5;
          else
            state_0x1d3 <= WAIT_WRITE_0x1C6;
          end if;
        when WAIT_WRITE_0x1C8 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x196 <= UNIT_VALUE;
            state_0x1d3 <= Q_0x1C7;
          else
            state_0x1d3 <= WAIT_WRITE_0x1C8;
          end if;
        when WAIT_WRITE_0x1CA =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x197 <= UNIT_VALUE;
            state_0x1d3 <= Q_0x1C9;
          else
            state_0x1d3 <= WAIT_WRITE_0x1CA;
          end if;
        when WAIT_WRITE_0x1CC =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x198 <= UNIT_VALUE;
            state_0x1d3 <= Q_0x1CB;
          else
            state_0x1d3 <= WAIT_WRITE_0x1CC;
          end if;
        when WAIT_WRITE_0x1CE =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x199 <= UNIT_VALUE;
            state_0x1d3 <= Q_0x1CD;
          else
            state_0x1d3 <= WAIT_WRITE_0x1CE;
          end if;
        when WAIT_WRITE_0x1D0 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x19a <= UNIT_VALUE;
            state_0x1d3 <= Q_0x1CF;
          else
            state_0x1d3 <= WAIT_WRITE_0x1D0;
          end if;
        when WAIT_WRITE_0x1D2 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x19b <= UNIT_VALUE;
            state_0x1d3 <= Q_0x1D1;
          else
            state_0x1d3 <= WAIT_WRITE_0x1D2;
          end if;
        when Q_0x1D1 =>
          idx_0x16b <= idx_0x16b + to_signed(8,31);
          state_0x1d3 <= AUX_0x16A;
        when Q_0x1CF =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x16b + to_signed(7,31));
          avm_wm_writedata <= std_logic_vector(element_0x193)& "1";
          avm_wm_write <= '1';
          state_0x1d3 <= WAIT_WRITE_0x1D2;
        when Q_0x1CD =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x16b + to_signed(6,31));
          avm_wm_writedata <= std_logic_vector(element_0x18f)& "1";
          avm_wm_write <= '1';
          state_0x1d3 <= WAIT_WRITE_0x1D0;
        when Q_0x1CB =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x16b + to_signed(5,31));
          avm_wm_writedata <= std_logic_vector(element_0x18b)& "1";
          avm_wm_write <= '1';
          state_0x1d3 <= WAIT_WRITE_0x1CE;
        when Q_0x1C9 =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x16b + to_signed(4,31));
          avm_wm_writedata <= std_logic_vector(element_0x187)& "1";
          avm_wm_write <= '1';
          state_0x1d3 <= WAIT_WRITE_0x1CC;
        when Q_0x1C7 =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x16b + to_signed(3,31));
          avm_wm_writedata <= std_logic_vector(element_0x183)& "1";
          avm_wm_write <= '1';
          state_0x1d3 <= WAIT_WRITE_0x1CA;
        when Q_0x1C5 =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x16b + to_signed(2,31));
          avm_wm_writedata <= std_logic_vector(element_0x17f)& "1";
          avm_wm_write <= '1';
          state_0x1d3 <= WAIT_WRITE_0x1C8;
        when Q_0x1C3 =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x16b + to_signed(1,31));
          avm_wm_writedata <= std_logic_vector(element_0x17b)& "1";
          avm_wm_write <= '1';
          state_0x1d3 <= WAIT_WRITE_0x1C6;
        when Q_0x1C0 =>
          start_l_0x1b9 <= '1';
          start_l_0x1ba <= '1';
          start_l_0x1bb <= '1';
          start_l_0x1bc <= '1';
          start_l_0x1bd <= '1';
          start_l_0x1be <= '1';
          start_l_0x1bf <= '1';
          state_0x1d3 <= P_0x1C1;
        when P_0x1C1 =>
          start_l_0x1b9 <= '0';
          start_l_0x1ba <= '0';
          start_l_0x1bb <= '0';
          start_l_0x1bc <= '0';
          start_l_0x1bd <= '0';
          start_l_0x1be <= '0';
          start_l_0x1bf <= '0';
          acc_0x175 <= to_signed(1,31);
          n_0x176 <= element_0x16c;
          state_0x1d3 <= ADD_0x174;
        when R_0x1C2 =>
          if ((((((rdy_l_0x1b9 = '1') and (rdy_l_0x1ba = '1')) and (rdy_l_0x1bb = '1')) and (rdy_l_0x1bc = '1')) and (rdy_l_0x1bd = '1')) and (rdy_l_0x1be = '1')) and (rdy_l_0x1bf = '1') then
            avm_wm_address <= compute_address(caml_heap_base, a, idx_0x16b + to_signed(0,31));
            avm_wm_writedata <= std_logic_vector(element_0x177)& "1";
            avm_wm_write <= '1';
            state_0x1d3 <= WAIT_WRITE_0x1C4;
          else
            state_0x1d3 <= R_0x1C2;
          end if;
        when Q_0x1B7 =>
          state_0x1d3 <= Q_0x1C0;
        when Q_0x1B5 =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x16b + to_signed(7,31));
          avm_rm_read <= '1';
          state_0x1d3 <= WAIT_READ_0x1B8;
        when Q_0x1B3 =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x16b + to_signed(6,31));
          avm_rm_read <= '1';
          state_0x1d3 <= WAIT_READ_0x1B6;
        when Q_0x1B1 =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x16b + to_signed(5,31));
          avm_rm_read <= '1';
          state_0x1d3 <= WAIT_READ_0x1B4;
        when Q_0x1AF =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x16b + to_signed(4,31));
          avm_rm_read <= '1';
          state_0x1d3 <= WAIT_READ_0x1B2;
        when Q_0x1AD =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x16b + to_signed(3,31));
          avm_rm_read <= '1';
          state_0x1d3 <= WAIT_READ_0x1B0;
        when Q_0x1AB =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x16b + to_signed(2,31));
          avm_rm_read <= '1';
          state_0x1d3 <= WAIT_READ_0x1AE;
        when Q_0x1A9 =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x16b + to_signed(1,31));
          avm_rm_read <= '1';
          state_0x1d3 <= WAIT_READ_0x1AC;
        when AUX_0x16A =>
          if idx_0x16b >= size_0x169 then
            result <= UNIT_VALUE;
            state_0x1d3 <= IDLE;
          elsif idx_0x16b > (size_0x169 - to_signed(8,31)) then
            avm_rm_address <= compute_address(caml_heap_base, a, idx_0x16b);
            avm_rm_read <= '1';
            state_0x1d3 <= WAIT_READ_0x1A5;
          else
            avm_rm_address <= compute_address(caml_heap_base, a, idx_0x16b + to_signed(0,31));
            avm_rm_read <= '1';
            state_0x1d3 <= WAIT_READ_0x1AA;
          end if;
        
      when Q_0x1A2 =>
        idx_0x16b <= to_signed(0,31);
        state_0x1d3 <= AUX_0x16A;
      when IDLE =>
        if start = '1' then
          rdy <= '0';
          avm_rm_address <= compute_address(caml_heap_base, a, to_signed(-1,31));
          avm_rm_read <= '1';
          state_0x1d3 <= WAIT_READ_0x1A3;
        else
          rdy <= '1';
          state_0x1d3 <= IDLE;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x1d4 <= IDLE_L_0x1B9;
  elsif rising_edge(clk) then
    case STATE_0x1d4 is
      when ADD_0x178 =>
        if n_0x17a <= to_signed(0,31) then
          element_0x17b <= acc_0x179;
          state_0x1d4 <= IDLE_L_0x1B9;
        else
          acc_0x179 <= acc_0x179 + to_signed(1,31);
          n_0x17a <= n_0x17a - to_signed(1,31);
          state_0x1d4 <= ADD_0x178;
        end if;
      when IDLE_L_0x1B9 =>
        if start_l_0x1b9 = '1' then
          rdy_l_0x1b9 <= '0';
          acc_0x179 <= to_signed(1,31);
          n_0x17a <= element_0x16d;
          state_0x1d4 <= ADD_0x178;
        else
          rdy_l_0x1b9 <= '1';
          state_0x1d4 <= IDLE_L_0x1B9;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x1d5 <= IDLE_L_0x1BA;
  elsif rising_edge(clk) then
    case STATE_0x1d5 is
      when ADD_0x17C =>
        if n_0x17e <= to_signed(0,31) then
          element_0x17f <= acc_0x17d;
          state_0x1d5 <= IDLE_L_0x1BA;
        else
          acc_0x17d <= acc_0x17d + to_signed(1,31);
          n_0x17e <= n_0x17e - to_signed(1,31);
          state_0x1d5 <= ADD_0x17C;
        end if;
      when IDLE_L_0x1BA =>
        if start_l_0x1ba = '1' then
          rdy_l_0x1ba <= '0';
          acc_0x17d <= to_signed(1,31);
          n_0x17e <= element_0x16e;
          state_0x1d5 <= ADD_0x17C;
        else
          rdy_l_0x1ba <= '1';
          state_0x1d5 <= IDLE_L_0x1BA;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x1d6 <= IDLE_L_0x1BB;
  elsif rising_edge(clk) then
    case STATE_0x1d6 is
      when ADD_0x180 =>
        if n_0x182 <= to_signed(0,31) then
          element_0x183 <= acc_0x181;
          state_0x1d6 <= IDLE_L_0x1BB;
        else
          acc_0x181 <= acc_0x181 + to_signed(1,31);
          n_0x182 <= n_0x182 - to_signed(1,31);
          state_0x1d6 <= ADD_0x180;
        end if;
      when IDLE_L_0x1BB =>
        if start_l_0x1bb = '1' then
          rdy_l_0x1bb <= '0';
          acc_0x181 <= to_signed(1,31);
          n_0x182 <= element_0x16f;
          state_0x1d6 <= ADD_0x180;
        else
          rdy_l_0x1bb <= '1';
          state_0x1d6 <= IDLE_L_0x1BB;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x1d7 <= IDLE_L_0x1BC;
  elsif rising_edge(clk) then
    case STATE_0x1d7 is
      when ADD_0x184 =>
        if n_0x186 <= to_signed(0,31) then
          element_0x187 <= acc_0x185;
          state_0x1d7 <= IDLE_L_0x1BC;
        else
          acc_0x185 <= acc_0x185 + to_signed(1,31);
          n_0x186 <= n_0x186 - to_signed(1,31);
          state_0x1d7 <= ADD_0x184;
        end if;
      when IDLE_L_0x1BC =>
        if start_l_0x1bc = '1' then
          rdy_l_0x1bc <= '0';
          acc_0x185 <= to_signed(1,31);
          n_0x186 <= element_0x170;
          state_0x1d7 <= ADD_0x184;
        else
          rdy_l_0x1bc <= '1';
          state_0x1d7 <= IDLE_L_0x1BC;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x1d8 <= IDLE_L_0x1BD;
  elsif rising_edge(clk) then
    case STATE_0x1d8 is
      when ADD_0x188 =>
        if n_0x18a <= to_signed(0,31) then
          element_0x18b <= acc_0x189;
          state_0x1d8 <= IDLE_L_0x1BD;
        else
          acc_0x189 <= acc_0x189 + to_signed(1,31);
          n_0x18a <= n_0x18a - to_signed(1,31);
          state_0x1d8 <= ADD_0x188;
        end if;
      when IDLE_L_0x1BD =>
        if start_l_0x1bd = '1' then
          rdy_l_0x1bd <= '0';
          acc_0x189 <= to_signed(1,31);
          n_0x18a <= element_0x171;
          state_0x1d8 <= ADD_0x188;
        else
          rdy_l_0x1bd <= '1';
          state_0x1d8 <= IDLE_L_0x1BD;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x1d9 <= IDLE_L_0x1BE;
  elsif rising_edge(clk) then
    case STATE_0x1d9 is
      when ADD_0x18C =>
        if n_0x18e <= to_signed(0,31) then
          element_0x18f <= acc_0x18d;
          state_0x1d9 <= IDLE_L_0x1BE;
        else
          acc_0x18d <= acc_0x18d + to_signed(1,31);
          n_0x18e <= n_0x18e - to_signed(1,31);
          state_0x1d9 <= ADD_0x18C;
        end if;
      when IDLE_L_0x1BE =>
        if start_l_0x1be = '1' then
          rdy_l_0x1be <= '0';
          acc_0x18d <= to_signed(1,31);
          n_0x18e <= element_0x172;
          state_0x1d9 <= ADD_0x18C;
        else
          rdy_l_0x1be <= '1';
          state_0x1d9 <= IDLE_L_0x1BE;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x1da <= IDLE_L_0x1BF;
  elsif rising_edge(clk) then
    case STATE_0x1da is
      when ADD_0x190 =>
        if n_0x192 <= to_signed(0,31) then
          element_0x193 <= acc_0x191;
          state_0x1da <= IDLE_L_0x1BF;
        else
          acc_0x191 <= acc_0x191 + to_signed(1,31);
          n_0x192 <= n_0x192 - to_signed(1,31);
          state_0x1da <= ADD_0x190;
        end if;
      when IDLE_L_0x1BF =>
        if start_l_0x1bf = '1' then
          rdy_l_0x1bf <= '0';
          acc_0x191 <= to_signed(1,31);
          n_0x192 <= element_0x173;
          state_0x1da <= ADD_0x190;
        else
          rdy_l_0x1bf <= '1';
          state_0x1da <= IDLE_L_0x1BF;
        end if;
      end case;
    end if;
  end process;

end architecture;
