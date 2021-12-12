library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.misc_map_heavy_inc16.all;

entity map_heavy_inc16 is
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
architecture RTL of map_heavy_inc16 is
  signal idx_0x277 : caml_int := to_signed(0,31);
  signal acc_0x2da : caml_int := to_signed(0,31);
  signal n_0x2db : caml_int := to_signed(0,31);
  signal x_0x2dc : caml_int := to_signed(0,31);
  signal ignore_0x2dd : unit := UNIT_VALUE;
  signal element_0x2d8 : caml_int := to_signed(0,31);
  signal acc_0x28d : caml_int := to_signed(0,31);
  signal n_0x28e : caml_int := to_signed(0,31);
  signal start_l_0x305 : std_logic := '-';
  signal rdy_l_0x305 : std_logic := '-';
  signal element_0x28f : caml_int := to_signed(0,31);
  signal acc_0x291 : caml_int := to_signed(0,31);
  signal n_0x292 : caml_int := to_signed(0,31);
  signal start_l_0x306 : std_logic := '-';
  signal rdy_l_0x306 : std_logic := '-';
  signal element_0x293 : caml_int := to_signed(0,31);
  signal acc_0x295 : caml_int := to_signed(0,31);
  signal n_0x296 : caml_int := to_signed(0,31);
  signal start_l_0x307 : std_logic := '-';
  signal rdy_l_0x307 : std_logic := '-';
  signal element_0x297 : caml_int := to_signed(0,31);
  signal acc_0x299 : caml_int := to_signed(0,31);
  signal n_0x29a : caml_int := to_signed(0,31);
  signal start_l_0x308 : std_logic := '-';
  signal rdy_l_0x308 : std_logic := '-';
  signal element_0x29b : caml_int := to_signed(0,31);
  signal acc_0x29d : caml_int := to_signed(0,31);
  signal n_0x29e : caml_int := to_signed(0,31);
  signal start_l_0x309 : std_logic := '-';
  signal rdy_l_0x309 : std_logic := '-';
  signal element_0x29f : caml_int := to_signed(0,31);
  signal acc_0x2a1 : caml_int := to_signed(0,31);
  signal n_0x2a2 : caml_int := to_signed(0,31);
  signal start_l_0x30a : std_logic := '-';
  signal rdy_l_0x30a : std_logic := '-';
  signal element_0x2a3 : caml_int := to_signed(0,31);
  signal acc_0x2a5 : caml_int := to_signed(0,31);
  signal n_0x2a6 : caml_int := to_signed(0,31);
  signal start_l_0x30b : std_logic := '-';
  signal rdy_l_0x30b : std_logic := '-';
  signal element_0x2a7 : caml_int := to_signed(0,31);
  signal acc_0x2a9 : caml_int := to_signed(0,31);
  signal n_0x2aa : caml_int := to_signed(0,31);
  signal start_l_0x30c : std_logic := '-';
  signal rdy_l_0x30c : std_logic := '-';
  signal element_0x2ab : caml_int := to_signed(0,31);
  signal acc_0x2ad : caml_int := to_signed(0,31);
  signal n_0x2ae : caml_int := to_signed(0,31);
  signal start_l_0x30d : std_logic := '-';
  signal rdy_l_0x30d : std_logic := '-';
  signal element_0x2af : caml_int := to_signed(0,31);
  signal acc_0x2b1 : caml_int := to_signed(0,31);
  signal n_0x2b2 : caml_int := to_signed(0,31);
  signal start_l_0x30e : std_logic := '-';
  signal rdy_l_0x30e : std_logic := '-';
  signal element_0x2b3 : caml_int := to_signed(0,31);
  signal acc_0x2b5 : caml_int := to_signed(0,31);
  signal n_0x2b6 : caml_int := to_signed(0,31);
  signal start_l_0x30f : std_logic := '-';
  signal rdy_l_0x30f : std_logic := '-';
  signal element_0x2b7 : caml_int := to_signed(0,31);
  signal acc_0x2b9 : caml_int := to_signed(0,31);
  signal n_0x2ba : caml_int := to_signed(0,31);
  signal start_l_0x310 : std_logic := '-';
  signal rdy_l_0x310 : std_logic := '-';
  signal element_0x2bb : caml_int := to_signed(0,31);
  signal acc_0x2bd : caml_int := to_signed(0,31);
  signal n_0x2be : caml_int := to_signed(0,31);
  signal start_l_0x311 : std_logic := '-';
  signal rdy_l_0x311 : std_logic := '-';
  signal element_0x2bf : caml_int := to_signed(0,31);
  signal acc_0x2c1 : caml_int := to_signed(0,31);
  signal n_0x2c2 : caml_int := to_signed(0,31);
  signal start_l_0x312 : std_logic := '-';
  signal rdy_l_0x312 : std_logic := '-';
  signal element_0x2c3 : caml_int := to_signed(0,31);
  signal acc_0x2c5 : caml_int := to_signed(0,31);
  signal n_0x2c6 : caml_int := to_signed(0,31);
  signal start_l_0x313 : std_logic := '-';
  signal rdy_l_0x313 : std_logic := '-';
  signal element_0x2c7 : caml_int := to_signed(0,31);
  signal acc_0x289 : caml_int := to_signed(0,31);
  signal n_0x28a : caml_int := to_signed(0,31);
  signal ignore_0x2d7 : unit := UNIT_VALUE;
  signal ignore_0x2d6 : unit := UNIT_VALUE;
  signal ignore_0x2d5 : unit := UNIT_VALUE;
  signal ignore_0x2d4 : unit := UNIT_VALUE;
  signal ignore_0x2d3 : unit := UNIT_VALUE;
  signal ignore_0x2d2 : unit := UNIT_VALUE;
  signal ignore_0x2d1 : unit := UNIT_VALUE;
  signal ignore_0x2d0 : unit := UNIT_VALUE;
  signal ignore_0x2cf : unit := UNIT_VALUE;
  signal ignore_0x2ce : unit := UNIT_VALUE;
  signal ignore_0x2cd : unit := UNIT_VALUE;
  signal ignore_0x2cc : unit := UNIT_VALUE;
  signal ignore_0x2cb : unit := UNIT_VALUE;
  signal ignore_0x2ca : unit := UNIT_VALUE;
  signal ignore_0x2c9 : unit := UNIT_VALUE;
  signal ignore_0x2c8 : unit := UNIT_VALUE;
  signal element_0x28b : caml_int := to_signed(0,31);
  signal element_0x287 : caml_int := to_signed(0,31);
  signal element_0x286 : caml_int := to_signed(0,31);
  signal element_0x285 : caml_int := to_signed(0,31);
  signal element_0x284 : caml_int := to_signed(0,31);
  signal element_0x283 : caml_int := to_signed(0,31);
  signal element_0x282 : caml_int := to_signed(0,31);
  signal element_0x281 : caml_int := to_signed(0,31);
  signal element_0x280 : caml_int := to_signed(0,31);
  signal element_0x27f : caml_int := to_signed(0,31);
  signal element_0x27e : caml_int := to_signed(0,31);
  signal element_0x27d : caml_int := to_signed(0,31);
  signal element_0x27c : caml_int := to_signed(0,31);
  signal element_0x27b : caml_int := to_signed(0,31);
  signal element_0x27a : caml_int := to_signed(0,31);
  signal element_0x279 : caml_int := to_signed(0,31);
  signal element_0x278 : caml_int := to_signed(0,31);
  signal size_0x275 : caml_int := to_signed(0,31);
  
  type STATE_0x337_T is (WAIT_READ_0x2DF, WAIT_READ_0x2E1, ADD_0x2D9,
                         WAIT_WRITE_0x2E4, Q_0x2E3, Q_0x2E2, Q_0x2E0,
                         WAIT_READ_0x2E6, WAIT_READ_0x2E8, WAIT_READ_0x2EA,
                         WAIT_READ_0x2EC, WAIT_READ_0x2EE, WAIT_READ_0x2F0,
                         WAIT_READ_0x2F2, WAIT_READ_0x2F4, WAIT_READ_0x2F6,
                         WAIT_READ_0x2F8, WAIT_READ_0x2FA, WAIT_READ_0x2FC,
                         WAIT_READ_0x2FE, WAIT_READ_0x300, WAIT_READ_0x302,
                         WAIT_READ_0x304, ADD_0x288, WAIT_WRITE_0x318,
                         WAIT_WRITE_0x31A, WAIT_WRITE_0x31C,
                         WAIT_WRITE_0x31E, WAIT_WRITE_0x320,
                         WAIT_WRITE_0x322, WAIT_WRITE_0x324,
                         WAIT_WRITE_0x326, WAIT_WRITE_0x328,
                         WAIT_WRITE_0x32A, WAIT_WRITE_0x32C,
                         WAIT_WRITE_0x32E, WAIT_WRITE_0x330,
                         WAIT_WRITE_0x332, WAIT_WRITE_0x334,
                         WAIT_WRITE_0x336, Q_0x335, Q_0x333, Q_0x331,
                         Q_0x32F, Q_0x32D, Q_0x32B, Q_0x329, Q_0x327,
                         Q_0x325, Q_0x323, Q_0x321, Q_0x31F, Q_0x31D,
                         Q_0x31B, Q_0x319, Q_0x317, Q_0x314, P_0x315,
                         R_0x316, Q_0x303, Q_0x301, Q_0x2FF, Q_0x2FD,
                         Q_0x2FB, Q_0x2F9, Q_0x2F7, Q_0x2F5, Q_0x2F3,
                         Q_0x2F1, Q_0x2EF, Q_0x2ED, Q_0x2EB, Q_0x2E9,
                         Q_0x2E7, Q_0x2E5, AUX_0x276, Q_0x2DE, IDLE);
  signal STATE_0x337 : STATE_0x337_T;
  
  type STATE_0x338_T is (ADD_0x28C, IDLE_L_0x305);
  signal STATE_0x338 : STATE_0x338_T;
  
  type STATE_0x339_T is (ADD_0x290, IDLE_L_0x306);
  signal STATE_0x339 : STATE_0x339_T;
  
  type STATE_0x33a_T is (ADD_0x294, IDLE_L_0x307);
  signal STATE_0x33a : STATE_0x33a_T;
  
  type STATE_0x33b_T is (ADD_0x298, IDLE_L_0x308);
  signal STATE_0x33b : STATE_0x33b_T;
  
  type STATE_0x33c_T is (ADD_0x29C, IDLE_L_0x309);
  signal STATE_0x33c : STATE_0x33c_T;
  
  type STATE_0x33d_T is (ADD_0x2A0, IDLE_L_0x30A);
  signal STATE_0x33d : STATE_0x33d_T;
  
  type STATE_0x33e_T is (ADD_0x2A4, IDLE_L_0x30B);
  signal STATE_0x33e : STATE_0x33e_T;
  
  type STATE_0x33f_T is (ADD_0x2A8, IDLE_L_0x30C);
  signal STATE_0x33f : STATE_0x33f_T;
  
  type STATE_0x340_T is (ADD_0x2AC, IDLE_L_0x30D);
  signal STATE_0x340 : STATE_0x340_T;
  
  type STATE_0x341_T is (ADD_0x2B0, IDLE_L_0x30E);
  signal STATE_0x341 : STATE_0x341_T;
  
  type STATE_0x342_T is (ADD_0x2B4, IDLE_L_0x30F);
  signal STATE_0x342 : STATE_0x342_T;
  
  type STATE_0x343_T is (ADD_0x2B8, IDLE_L_0x310);
  signal STATE_0x343 : STATE_0x343_T;
  
  type STATE_0x344_T is (ADD_0x2BC, IDLE_L_0x311);
  signal STATE_0x344 : STATE_0x344_T;
  
  type STATE_0x345_T is (ADD_0x2C0, IDLE_L_0x312);
  signal STATE_0x345 : STATE_0x345_T;
  
  type STATE_0x346_T is (ADD_0x2C4, IDLE_L_0x313);
  signal STATE_0x346 : STATE_0x346_T;
  
begin
  process(reset,clk) begin
    if reset = '1' then
      state_0x337 <= IDLE;
    elsif rising_edge(clk) then
      case STATE_0x337 is
        when WAIT_READ_0x2DF =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            size_0x275 <= size_header(avm_rm_readdata);
            state_0x337 <= Q_0x2DE;
          else
            state_0x337 <= WAIT_READ_0x2DF;
          end if;
        when WAIT_READ_0x2E1 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x2d8 <= signed(avm_rm_readdata(31 downto 1));
            state_0x337 <= Q_0x2E0;
          else
            state_0x337 <= WAIT_READ_0x2E1;
          end if;
        when ADD_0x2D9 =>
          if n_0x2db <= to_signed(0,31) then
            x_0x2dc <= acc_0x2da;
            state_0x337 <= Q_0x2E3;
          else
            acc_0x2da <= acc_0x2da + to_signed(1,31);
            n_0x2db <= n_0x2db - to_signed(1,31);
            state_0x337 <= ADD_0x2D9;
          end if;
        when WAIT_WRITE_0x2E4 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x2dd <= UNIT_VALUE;
            state_0x337 <= Q_0x2E2;
          else
            state_0x337 <= WAIT_WRITE_0x2E4;
          end if;
        when Q_0x2E3 =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x277);
          avm_wm_writedata <= std_logic_vector(x_0x2dc)& "1";
          avm_wm_write <= '1';
          state_0x337 <= WAIT_WRITE_0x2E4;
        when Q_0x2E2 =>
          idx_0x277 <= idx_0x277 + to_signed(1,31);
          state_0x337 <= AUX_0x276;
        when Q_0x2E0 =>
          acc_0x2da <= to_signed(1,31);
          n_0x2db <= element_0x2d8;
          state_0x337 <= ADD_0x2D9;
        when WAIT_READ_0x2E6 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x278 <= signed(avm_rm_readdata(31 downto 1));
            state_0x337 <= Q_0x2E5;
          else
            state_0x337 <= WAIT_READ_0x2E6;
          end if;
        when WAIT_READ_0x2E8 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x279 <= signed(avm_rm_readdata(31 downto 1));
            state_0x337 <= Q_0x2E7;
          else
            state_0x337 <= WAIT_READ_0x2E8;
          end if;
        when WAIT_READ_0x2EA =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x27a <= signed(avm_rm_readdata(31 downto 1));
            state_0x337 <= Q_0x2E9;
          else
            state_0x337 <= WAIT_READ_0x2EA;
          end if;
        when WAIT_READ_0x2EC =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x27b <= signed(avm_rm_readdata(31 downto 1));
            state_0x337 <= Q_0x2EB;
          else
            state_0x337 <= WAIT_READ_0x2EC;
          end if;
        when WAIT_READ_0x2EE =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x27c <= signed(avm_rm_readdata(31 downto 1));
            state_0x337 <= Q_0x2ED;
          else
            state_0x337 <= WAIT_READ_0x2EE;
          end if;
        when WAIT_READ_0x2F0 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x27d <= signed(avm_rm_readdata(31 downto 1));
            state_0x337 <= Q_0x2EF;
          else
            state_0x337 <= WAIT_READ_0x2F0;
          end if;
        when WAIT_READ_0x2F2 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x27e <= signed(avm_rm_readdata(31 downto 1));
            state_0x337 <= Q_0x2F1;
          else
            state_0x337 <= WAIT_READ_0x2F2;
          end if;
        when WAIT_READ_0x2F4 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x27f <= signed(avm_rm_readdata(31 downto 1));
            state_0x337 <= Q_0x2F3;
          else
            state_0x337 <= WAIT_READ_0x2F4;
          end if;
        when WAIT_READ_0x2F6 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x280 <= signed(avm_rm_readdata(31 downto 1));
            state_0x337 <= Q_0x2F5;
          else
            state_0x337 <= WAIT_READ_0x2F6;
          end if;
        when WAIT_READ_0x2F8 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x281 <= signed(avm_rm_readdata(31 downto 1));
            state_0x337 <= Q_0x2F7;
          else
            state_0x337 <= WAIT_READ_0x2F8;
          end if;
        when WAIT_READ_0x2FA =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x282 <= signed(avm_rm_readdata(31 downto 1));
            state_0x337 <= Q_0x2F9;
          else
            state_0x337 <= WAIT_READ_0x2FA;
          end if;
        when WAIT_READ_0x2FC =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x283 <= signed(avm_rm_readdata(31 downto 1));
            state_0x337 <= Q_0x2FB;
          else
            state_0x337 <= WAIT_READ_0x2FC;
          end if;
        when WAIT_READ_0x2FE =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x284 <= signed(avm_rm_readdata(31 downto 1));
            state_0x337 <= Q_0x2FD;
          else
            state_0x337 <= WAIT_READ_0x2FE;
          end if;
        when WAIT_READ_0x300 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x285 <= signed(avm_rm_readdata(31 downto 1));
            state_0x337 <= Q_0x2FF;
          else
            state_0x337 <= WAIT_READ_0x300;
          end if;
        when WAIT_READ_0x302 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x286 <= signed(avm_rm_readdata(31 downto 1));
            state_0x337 <= Q_0x301;
          else
            state_0x337 <= WAIT_READ_0x302;
          end if;
        when WAIT_READ_0x304 =>
          if avm_rm_waitrequest = '0' then
            avm_rm_read <= '0';
            element_0x287 <= signed(avm_rm_readdata(31 downto 1));
            state_0x337 <= Q_0x303;
          else
            state_0x337 <= WAIT_READ_0x304;
          end if;
        when ADD_0x288 =>
          if n_0x28a <= to_signed(0,31) then
            element_0x28b <= acc_0x289;
            state_0x337 <= R_0x316;
          else
            acc_0x289 <= acc_0x289 + to_signed(1,31);
            n_0x28a <= n_0x28a - to_signed(1,31);
            state_0x337 <= ADD_0x288;
          end if;
        when WAIT_WRITE_0x318 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x2c8 <= UNIT_VALUE;
            state_0x337 <= Q_0x317;
          else
            state_0x337 <= WAIT_WRITE_0x318;
          end if;
        when WAIT_WRITE_0x31A =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x2c9 <= UNIT_VALUE;
            state_0x337 <= Q_0x319;
          else
            state_0x337 <= WAIT_WRITE_0x31A;
          end if;
        when WAIT_WRITE_0x31C =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x2ca <= UNIT_VALUE;
            state_0x337 <= Q_0x31B;
          else
            state_0x337 <= WAIT_WRITE_0x31C;
          end if;
        when WAIT_WRITE_0x31E =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x2cb <= UNIT_VALUE;
            state_0x337 <= Q_0x31D;
          else
            state_0x337 <= WAIT_WRITE_0x31E;
          end if;
        when WAIT_WRITE_0x320 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x2cc <= UNIT_VALUE;
            state_0x337 <= Q_0x31F;
          else
            state_0x337 <= WAIT_WRITE_0x320;
          end if;
        when WAIT_WRITE_0x322 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x2cd <= UNIT_VALUE;
            state_0x337 <= Q_0x321;
          else
            state_0x337 <= WAIT_WRITE_0x322;
          end if;
        when WAIT_WRITE_0x324 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x2ce <= UNIT_VALUE;
            state_0x337 <= Q_0x323;
          else
            state_0x337 <= WAIT_WRITE_0x324;
          end if;
        when WAIT_WRITE_0x326 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x2cf <= UNIT_VALUE;
            state_0x337 <= Q_0x325;
          else
            state_0x337 <= WAIT_WRITE_0x326;
          end if;
        when WAIT_WRITE_0x328 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x2d0 <= UNIT_VALUE;
            state_0x337 <= Q_0x327;
          else
            state_0x337 <= WAIT_WRITE_0x328;
          end if;
        when WAIT_WRITE_0x32A =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x2d1 <= UNIT_VALUE;
            state_0x337 <= Q_0x329;
          else
            state_0x337 <= WAIT_WRITE_0x32A;
          end if;
        when WAIT_WRITE_0x32C =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x2d2 <= UNIT_VALUE;
            state_0x337 <= Q_0x32B;
          else
            state_0x337 <= WAIT_WRITE_0x32C;
          end if;
        when WAIT_WRITE_0x32E =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x2d3 <= UNIT_VALUE;
            state_0x337 <= Q_0x32D;
          else
            state_0x337 <= WAIT_WRITE_0x32E;
          end if;
        when WAIT_WRITE_0x330 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x2d4 <= UNIT_VALUE;
            state_0x337 <= Q_0x32F;
          else
            state_0x337 <= WAIT_WRITE_0x330;
          end if;
        when WAIT_WRITE_0x332 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x2d5 <= UNIT_VALUE;
            state_0x337 <= Q_0x331;
          else
            state_0x337 <= WAIT_WRITE_0x332;
          end if;
        when WAIT_WRITE_0x334 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x2d6 <= UNIT_VALUE;
            state_0x337 <= Q_0x333;
          else
            state_0x337 <= WAIT_WRITE_0x334;
          end if;
        when WAIT_WRITE_0x336 =>
          if avm_wm_waitrequest = '0' then
            avm_wm_write <= '0';
            ignore_0x2d7 <= UNIT_VALUE;
            state_0x337 <= Q_0x335;
          else
            state_0x337 <= WAIT_WRITE_0x336;
          end if;
        when Q_0x335 =>
          idx_0x277 <= idx_0x277 + to_signed(16,31);
          state_0x337 <= AUX_0x276;
        when Q_0x333 =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(15,31));
          avm_wm_writedata <= std_logic_vector(element_0x2c7)& "1";
          avm_wm_write <= '1';
          state_0x337 <= WAIT_WRITE_0x336;
        when Q_0x331 =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(14,31));
          avm_wm_writedata <= std_logic_vector(element_0x2c3)& "1";
          avm_wm_write <= '1';
          state_0x337 <= WAIT_WRITE_0x334;
        when Q_0x32F =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(13,31));
          avm_wm_writedata <= std_logic_vector(element_0x2bf)& "1";
          avm_wm_write <= '1';
          state_0x337 <= WAIT_WRITE_0x332;
        when Q_0x32D =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(12,31));
          avm_wm_writedata <= std_logic_vector(element_0x2bb)& "1";
          avm_wm_write <= '1';
          state_0x337 <= WAIT_WRITE_0x330;
        when Q_0x32B =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(11,31));
          avm_wm_writedata <= std_logic_vector(element_0x2b7)& "1";
          avm_wm_write <= '1';
          state_0x337 <= WAIT_WRITE_0x32E;
        when Q_0x329 =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(10,31));
          avm_wm_writedata <= std_logic_vector(element_0x2b3)& "1";
          avm_wm_write <= '1';
          state_0x337 <= WAIT_WRITE_0x32C;
        when Q_0x327 =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(9,31));
          avm_wm_writedata <= std_logic_vector(element_0x2af)& "1";
          avm_wm_write <= '1';
          state_0x337 <= WAIT_WRITE_0x32A;
        when Q_0x325 =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(8,31));
          avm_wm_writedata <= std_logic_vector(element_0x2ab)& "1";
          avm_wm_write <= '1';
          state_0x337 <= WAIT_WRITE_0x328;
        when Q_0x323 =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(7,31));
          avm_wm_writedata <= std_logic_vector(element_0x2a7)& "1";
          avm_wm_write <= '1';
          state_0x337 <= WAIT_WRITE_0x326;
        when Q_0x321 =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(6,31));
          avm_wm_writedata <= std_logic_vector(element_0x2a3)& "1";
          avm_wm_write <= '1';
          state_0x337 <= WAIT_WRITE_0x324;
        when Q_0x31F =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(5,31));
          avm_wm_writedata <= std_logic_vector(element_0x29f)& "1";
          avm_wm_write <= '1';
          state_0x337 <= WAIT_WRITE_0x322;
        when Q_0x31D =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(4,31));
          avm_wm_writedata <= std_logic_vector(element_0x29b)& "1";
          avm_wm_write <= '1';
          state_0x337 <= WAIT_WRITE_0x320;
        when Q_0x31B =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(3,31));
          avm_wm_writedata <= std_logic_vector(element_0x297)& "1";
          avm_wm_write <= '1';
          state_0x337 <= WAIT_WRITE_0x31E;
        when Q_0x319 =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(2,31));
          avm_wm_writedata <= std_logic_vector(element_0x293)& "1";
          avm_wm_write <= '1';
          state_0x337 <= WAIT_WRITE_0x31C;
        when Q_0x317 =>
          avm_wm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(1,31));
          avm_wm_writedata <= std_logic_vector(element_0x28f)& "1";
          avm_wm_write <= '1';
          state_0x337 <= WAIT_WRITE_0x31A;
        when Q_0x314 =>
          start_l_0x305 <= '1';
          start_l_0x306 <= '1';
          start_l_0x307 <= '1';
          start_l_0x308 <= '1';
          start_l_0x309 <= '1';
          start_l_0x30a <= '1';
          start_l_0x30b <= '1';
          start_l_0x30c <= '1';
          start_l_0x30d <= '1';
          start_l_0x30e <= '1';
          start_l_0x30f <= '1';
          start_l_0x310 <= '1';
          start_l_0x311 <= '1';
          start_l_0x312 <= '1';
          start_l_0x313 <= '1';
          state_0x337 <= P_0x315;
        when P_0x315 =>
          start_l_0x305 <= '0';
          start_l_0x306 <= '0';
          start_l_0x307 <= '0';
          start_l_0x308 <= '0';
          start_l_0x309 <= '0';
          start_l_0x30a <= '0';
          start_l_0x30b <= '0';
          start_l_0x30c <= '0';
          start_l_0x30d <= '0';
          start_l_0x30e <= '0';
          start_l_0x30f <= '0';
          start_l_0x310 <= '0';
          start_l_0x311 <= '0';
          start_l_0x312 <= '0';
          start_l_0x313 <= '0';
          acc_0x289 <= to_signed(1,31);
          n_0x28a <= element_0x278;
          state_0x337 <= ADD_0x288;
        when R_0x316 =>
          if ((((((((((((((rdy_l_0x305 = '1') and (rdy_l_0x306 = '1')) and (rdy_l_0x307 = '1')) and (rdy_l_0x308 = '1')) and (rdy_l_0x309 = '1')) and (rdy_l_0x30a = '1')) and (rdy_l_0x30b = '1')) and (rdy_l_0x30c = '1')) and (rdy_l_0x30d = '1')) and (rdy_l_0x30e = '1')) and (rdy_l_0x30f = '1')) and (rdy_l_0x310 = '1')) and (rdy_l_0x311 = '1')) and (rdy_l_0x312 = '1')) and (rdy_l_0x313 = '1') then
            avm_wm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(0,31));
            avm_wm_writedata <= std_logic_vector(element_0x28b)& "1";
            avm_wm_write <= '1';
            state_0x337 <= WAIT_WRITE_0x318;
          else
            state_0x337 <= R_0x316;
          end if;
        when Q_0x303 =>
          state_0x337 <= Q_0x314;
        when Q_0x301 =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(15,31));
          avm_rm_read <= '1';
          state_0x337 <= WAIT_READ_0x304;
        when Q_0x2FF =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(14,31));
          avm_rm_read <= '1';
          state_0x337 <= WAIT_READ_0x302;
        when Q_0x2FD =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(13,31));
          avm_rm_read <= '1';
          state_0x337 <= WAIT_READ_0x300;
        when Q_0x2FB =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(12,31));
          avm_rm_read <= '1';
          state_0x337 <= WAIT_READ_0x2FE;
        when Q_0x2F9 =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(11,31));
          avm_rm_read <= '1';
          state_0x337 <= WAIT_READ_0x2FC;
        when Q_0x2F7 =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(10,31));
          avm_rm_read <= '1';
          state_0x337 <= WAIT_READ_0x2FA;
        when Q_0x2F5 =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(9,31));
          avm_rm_read <= '1';
          state_0x337 <= WAIT_READ_0x2F8;
        when Q_0x2F3 =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(8,31));
          avm_rm_read <= '1';
          state_0x337 <= WAIT_READ_0x2F6;
        when Q_0x2F1 =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(7,31));
          avm_rm_read <= '1';
          state_0x337 <= WAIT_READ_0x2F4;
        when Q_0x2EF =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(6,31));
          avm_rm_read <= '1';
          state_0x337 <= WAIT_READ_0x2F2;
        when Q_0x2ED =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(5,31));
          avm_rm_read <= '1';
          state_0x337 <= WAIT_READ_0x2F0;
        when Q_0x2EB =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(4,31));
          avm_rm_read <= '1';
          state_0x337 <= WAIT_READ_0x2EE;
        when Q_0x2E9 =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(3,31));
          avm_rm_read <= '1';
          state_0x337 <= WAIT_READ_0x2EC;
        when Q_0x2E7 =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(2,31));
          avm_rm_read <= '1';
          state_0x337 <= WAIT_READ_0x2EA;
        when Q_0x2E5 =>
          avm_rm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(1,31));
          avm_rm_read <= '1';
          state_0x337 <= WAIT_READ_0x2E8;
        when AUX_0x276 =>
          if idx_0x277 >= size_0x275 then
            result <= UNIT_VALUE;
            state_0x337 <= IDLE;
          elsif idx_0x277 > (size_0x275 - to_signed(16,31)) then
            avm_rm_address <= compute_address(caml_heap_base, a, idx_0x277);
            avm_rm_read <= '1';
            state_0x337 <= WAIT_READ_0x2E1;
          else
            avm_rm_address <= compute_address(caml_heap_base, a, idx_0x277 + to_signed(0,31));
            avm_rm_read <= '1';
            state_0x337 <= WAIT_READ_0x2E6;
          end if;
        
      when Q_0x2DE =>
        idx_0x277 <= to_signed(0,31);
        state_0x337 <= AUX_0x276;
      when IDLE =>
        if start = '1' then
          rdy <= '0';
          avm_rm_address <= compute_address(caml_heap_base, a, to_signed(-1,31));
          avm_rm_read <= '1';
          state_0x337 <= WAIT_READ_0x2DF;
        else
          rdy <= '1';
          state_0x337 <= IDLE;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x338 <= IDLE_L_0x305;
  elsif rising_edge(clk) then
    case STATE_0x338 is
      when ADD_0x28C =>
        if n_0x28e <= to_signed(0,31) then
          element_0x28f <= acc_0x28d;
          state_0x338 <= IDLE_L_0x305;
        else
          acc_0x28d <= acc_0x28d + to_signed(1,31);
          n_0x28e <= n_0x28e - to_signed(1,31);
          state_0x338 <= ADD_0x28C;
        end if;
      when IDLE_L_0x305 =>
        if start_l_0x305 = '1' then
          rdy_l_0x305 <= '0';
          acc_0x28d <= to_signed(1,31);
          n_0x28e <= element_0x279;
          state_0x338 <= ADD_0x28C;
        else
          rdy_l_0x305 <= '1';
          state_0x338 <= IDLE_L_0x305;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x339 <= IDLE_L_0x306;
  elsif rising_edge(clk) then
    case STATE_0x339 is
      when ADD_0x290 =>
        if n_0x292 <= to_signed(0,31) then
          element_0x293 <= acc_0x291;
          state_0x339 <= IDLE_L_0x306;
        else
          acc_0x291 <= acc_0x291 + to_signed(1,31);
          n_0x292 <= n_0x292 - to_signed(1,31);
          state_0x339 <= ADD_0x290;
        end if;
      when IDLE_L_0x306 =>
        if start_l_0x306 = '1' then
          rdy_l_0x306 <= '0';
          acc_0x291 <= to_signed(1,31);
          n_0x292 <= element_0x27a;
          state_0x339 <= ADD_0x290;
        else
          rdy_l_0x306 <= '1';
          state_0x339 <= IDLE_L_0x306;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x33a <= IDLE_L_0x307;
  elsif rising_edge(clk) then
    case STATE_0x33a is
      when ADD_0x294 =>
        if n_0x296 <= to_signed(0,31) then
          element_0x297 <= acc_0x295;
          state_0x33a <= IDLE_L_0x307;
        else
          acc_0x295 <= acc_0x295 + to_signed(1,31);
          n_0x296 <= n_0x296 - to_signed(1,31);
          state_0x33a <= ADD_0x294;
        end if;
      when IDLE_L_0x307 =>
        if start_l_0x307 = '1' then
          rdy_l_0x307 <= '0';
          acc_0x295 <= to_signed(1,31);
          n_0x296 <= element_0x27b;
          state_0x33a <= ADD_0x294;
        else
          rdy_l_0x307 <= '1';
          state_0x33a <= IDLE_L_0x307;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x33b <= IDLE_L_0x308;
  elsif rising_edge(clk) then
    case STATE_0x33b is
      when ADD_0x298 =>
        if n_0x29a <= to_signed(0,31) then
          element_0x29b <= acc_0x299;
          state_0x33b <= IDLE_L_0x308;
        else
          acc_0x299 <= acc_0x299 + to_signed(1,31);
          n_0x29a <= n_0x29a - to_signed(1,31);
          state_0x33b <= ADD_0x298;
        end if;
      when IDLE_L_0x308 =>
        if start_l_0x308 = '1' then
          rdy_l_0x308 <= '0';
          acc_0x299 <= to_signed(1,31);
          n_0x29a <= element_0x27c;
          state_0x33b <= ADD_0x298;
        else
          rdy_l_0x308 <= '1';
          state_0x33b <= IDLE_L_0x308;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x33c <= IDLE_L_0x309;
  elsif rising_edge(clk) then
    case STATE_0x33c is
      when ADD_0x29C =>
        if n_0x29e <= to_signed(0,31) then
          element_0x29f <= acc_0x29d;
          state_0x33c <= IDLE_L_0x309;
        else
          acc_0x29d <= acc_0x29d + to_signed(1,31);
          n_0x29e <= n_0x29e - to_signed(1,31);
          state_0x33c <= ADD_0x29C;
        end if;
      when IDLE_L_0x309 =>
        if start_l_0x309 = '1' then
          rdy_l_0x309 <= '0';
          acc_0x29d <= to_signed(1,31);
          n_0x29e <= element_0x27d;
          state_0x33c <= ADD_0x29C;
        else
          rdy_l_0x309 <= '1';
          state_0x33c <= IDLE_L_0x309;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x33d <= IDLE_L_0x30A;
  elsif rising_edge(clk) then
    case STATE_0x33d is
      when ADD_0x2A0 =>
        if n_0x2a2 <= to_signed(0,31) then
          element_0x2a3 <= acc_0x2a1;
          state_0x33d <= IDLE_L_0x30A;
        else
          acc_0x2a1 <= acc_0x2a1 + to_signed(1,31);
          n_0x2a2 <= n_0x2a2 - to_signed(1,31);
          state_0x33d <= ADD_0x2A0;
        end if;
      when IDLE_L_0x30A =>
        if start_l_0x30a = '1' then
          rdy_l_0x30a <= '0';
          acc_0x2a1 <= to_signed(1,31);
          n_0x2a2 <= element_0x27e;
          state_0x33d <= ADD_0x2A0;
        else
          rdy_l_0x30a <= '1';
          state_0x33d <= IDLE_L_0x30A;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x33e <= IDLE_L_0x30B;
  elsif rising_edge(clk) then
    case STATE_0x33e is
      when ADD_0x2A4 =>
        if n_0x2a6 <= to_signed(0,31) then
          element_0x2a7 <= acc_0x2a5;
          state_0x33e <= IDLE_L_0x30B;
        else
          acc_0x2a5 <= acc_0x2a5 + to_signed(1,31);
          n_0x2a6 <= n_0x2a6 - to_signed(1,31);
          state_0x33e <= ADD_0x2A4;
        end if;
      when IDLE_L_0x30B =>
        if start_l_0x30b = '1' then
          rdy_l_0x30b <= '0';
          acc_0x2a5 <= to_signed(1,31);
          n_0x2a6 <= element_0x27f;
          state_0x33e <= ADD_0x2A4;
        else
          rdy_l_0x30b <= '1';
          state_0x33e <= IDLE_L_0x30B;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x33f <= IDLE_L_0x30C;
  elsif rising_edge(clk) then
    case STATE_0x33f is
      when ADD_0x2A8 =>
        if n_0x2aa <= to_signed(0,31) then
          element_0x2ab <= acc_0x2a9;
          state_0x33f <= IDLE_L_0x30C;
        else
          acc_0x2a9 <= acc_0x2a9 + to_signed(1,31);
          n_0x2aa <= n_0x2aa - to_signed(1,31);
          state_0x33f <= ADD_0x2A8;
        end if;
      when IDLE_L_0x30C =>
        if start_l_0x30c = '1' then
          rdy_l_0x30c <= '0';
          acc_0x2a9 <= to_signed(1,31);
          n_0x2aa <= element_0x280;
          state_0x33f <= ADD_0x2A8;
        else
          rdy_l_0x30c <= '1';
          state_0x33f <= IDLE_L_0x30C;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x340 <= IDLE_L_0x30D;
  elsif rising_edge(clk) then
    case STATE_0x340 is
      when ADD_0x2AC =>
        if n_0x2ae <= to_signed(0,31) then
          element_0x2af <= acc_0x2ad;
          state_0x340 <= IDLE_L_0x30D;
        else
          acc_0x2ad <= acc_0x2ad + to_signed(1,31);
          n_0x2ae <= n_0x2ae - to_signed(1,31);
          state_0x340 <= ADD_0x2AC;
        end if;
      when IDLE_L_0x30D =>
        if start_l_0x30d = '1' then
          rdy_l_0x30d <= '0';
          acc_0x2ad <= to_signed(1,31);
          n_0x2ae <= element_0x281;
          state_0x340 <= ADD_0x2AC;
        else
          rdy_l_0x30d <= '1';
          state_0x340 <= IDLE_L_0x30D;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x341 <= IDLE_L_0x30E;
  elsif rising_edge(clk) then
    case STATE_0x341 is
      when ADD_0x2B0 =>
        if n_0x2b2 <= to_signed(0,31) then
          element_0x2b3 <= acc_0x2b1;
          state_0x341 <= IDLE_L_0x30E;
        else
          acc_0x2b1 <= acc_0x2b1 + to_signed(1,31);
          n_0x2b2 <= n_0x2b2 - to_signed(1,31);
          state_0x341 <= ADD_0x2B0;
        end if;
      when IDLE_L_0x30E =>
        if start_l_0x30e = '1' then
          rdy_l_0x30e <= '0';
          acc_0x2b1 <= to_signed(1,31);
          n_0x2b2 <= element_0x282;
          state_0x341 <= ADD_0x2B0;
        else
          rdy_l_0x30e <= '1';
          state_0x341 <= IDLE_L_0x30E;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x342 <= IDLE_L_0x30F;
  elsif rising_edge(clk) then
    case STATE_0x342 is
      when ADD_0x2B4 =>
        if n_0x2b6 <= to_signed(0,31) then
          element_0x2b7 <= acc_0x2b5;
          state_0x342 <= IDLE_L_0x30F;
        else
          acc_0x2b5 <= acc_0x2b5 + to_signed(1,31);
          n_0x2b6 <= n_0x2b6 - to_signed(1,31);
          state_0x342 <= ADD_0x2B4;
        end if;
      when IDLE_L_0x30F =>
        if start_l_0x30f = '1' then
          rdy_l_0x30f <= '0';
          acc_0x2b5 <= to_signed(1,31);
          n_0x2b6 <= element_0x283;
          state_0x342 <= ADD_0x2B4;
        else
          rdy_l_0x30f <= '1';
          state_0x342 <= IDLE_L_0x30F;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x343 <= IDLE_L_0x310;
  elsif rising_edge(clk) then
    case STATE_0x343 is
      when ADD_0x2B8 =>
        if n_0x2ba <= to_signed(0,31) then
          element_0x2bb <= acc_0x2b9;
          state_0x343 <= IDLE_L_0x310;
        else
          acc_0x2b9 <= acc_0x2b9 + to_signed(1,31);
          n_0x2ba <= n_0x2ba - to_signed(1,31);
          state_0x343 <= ADD_0x2B8;
        end if;
      when IDLE_L_0x310 =>
        if start_l_0x310 = '1' then
          rdy_l_0x310 <= '0';
          acc_0x2b9 <= to_signed(1,31);
          n_0x2ba <= element_0x284;
          state_0x343 <= ADD_0x2B8;
        else
          rdy_l_0x310 <= '1';
          state_0x343 <= IDLE_L_0x310;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x344 <= IDLE_L_0x311;
  elsif rising_edge(clk) then
    case STATE_0x344 is
      when ADD_0x2BC =>
        if n_0x2be <= to_signed(0,31) then
          element_0x2bf <= acc_0x2bd;
          state_0x344 <= IDLE_L_0x311;
        else
          acc_0x2bd <= acc_0x2bd + to_signed(1,31);
          n_0x2be <= n_0x2be - to_signed(1,31);
          state_0x344 <= ADD_0x2BC;
        end if;
      when IDLE_L_0x311 =>
        if start_l_0x311 = '1' then
          rdy_l_0x311 <= '0';
          acc_0x2bd <= to_signed(1,31);
          n_0x2be <= element_0x285;
          state_0x344 <= ADD_0x2BC;
        else
          rdy_l_0x311 <= '1';
          state_0x344 <= IDLE_L_0x311;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x345 <= IDLE_L_0x312;
  elsif rising_edge(clk) then
    case STATE_0x345 is
      when ADD_0x2C0 =>
        if n_0x2c2 <= to_signed(0,31) then
          element_0x2c3 <= acc_0x2c1;
          state_0x345 <= IDLE_L_0x312;
        else
          acc_0x2c1 <= acc_0x2c1 + to_signed(1,31);
          n_0x2c2 <= n_0x2c2 - to_signed(1,31);
          state_0x345 <= ADD_0x2C0;
        end if;
      when IDLE_L_0x312 =>
        if start_l_0x312 = '1' then
          rdy_l_0x312 <= '0';
          acc_0x2c1 <= to_signed(1,31);
          n_0x2c2 <= element_0x286;
          state_0x345 <= ADD_0x2C0;
        else
          rdy_l_0x312 <= '1';
          state_0x345 <= IDLE_L_0x312;
        end if;
      end case;
    end if;
  end process;
process(reset,clk) begin
  if reset = '1' then
    state_0x346 <= IDLE_L_0x313;
  elsif rising_edge(clk) then
    case STATE_0x346 is
      when ADD_0x2C4 =>
        if n_0x2c6 <= to_signed(0,31) then
          element_0x2c7 <= acc_0x2c5;
          state_0x346 <= IDLE_L_0x313;
        else
          acc_0x2c5 <= acc_0x2c5 + to_signed(1,31);
          n_0x2c6 <= n_0x2c6 - to_signed(1,31);
          state_0x346 <= ADD_0x2C4;
        end if;
      when IDLE_L_0x313 =>
        if start_l_0x313 = '1' then
          rdy_l_0x313 <= '0';
          acc_0x2c5 <= to_signed(1,31);
          n_0x2c6 <= element_0x287;
          state_0x346 <= ADD_0x2C4;
        else
          rdy_l_0x313 <= '1';
          state_0x346 <= IDLE_L_0x313;
        end if;
      end case;
    end if;
  end process;

end architecture;
