# Macle (automata-based applicative language)

A Tool dedicated to FPGA programming, directly usable in combination with [O2B](https://github.com/jserot/O2B), an implementation of the OCaml Virtual Machine.

### dependencies

- OCaml >= 4.09
- opam install menhir

### Syntax

- circuit
```
pi ::= circuit x(x1,... xn) = e 
e ::= c | x | x(e1,... en) | let rec f(x1,...xn) = e and ... in e
    | if e1 then e2 else e3
    | let x = e and ... in e
    | <unop> e
    | e1 <binop> e2
```
- exemple 

```
(* Macle declarations *)

circuit f(a,b) = 
  let rec fact(n) =
    let rec aux(acc,n) = 
      if n <= 0 then acc else aux(acc*n,n-1) in
      aux(1,n) 
  in
  fact(a) + fact(b)

;;; 

(* OCaml program *)

print_int @@ f 5 6;;
```

### Usage

- code generation (VHDL)

```shell

$ make
$ ./compile bench/par/fact_par.ml -simul -time

- process bench/par/fact_par.ml:
let f (a : int) (b : int) =
  (let rec fact (n : int) = 
     (let rec aux (acc : int) (n : int) = 
        (if (n <= 0)
        then acc
        else (aux (acc * n) (n - 1)))
        in (aux 1 n)
     )in ((fact a) + (fact b)
))
  ;; (* OCaml program *)
  

print_int @@ f 5 6;;
  info: circuit  "f"  generated in folder gen/.
$
```

produces (among others) the file :

```
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.misc_f.all;

entity f is
  port(signal clk : in std_logic;
       signal reset : in std_logic;
       signal a : in caml_int;
       signal b : in caml_int;
       signal start : in std_logic;
       signal rdy : out std_logic;
       signal result : out caml_int);
end entity;
architecture RTL of f is
  signal start_l_0018 : std_logic := '-';
  signal rdy_l_0018 : std_logic := '-';
  signal tmp_0007_0010 : caml_int := to_signed(0,31);
  signal tmp_0006_0009 : caml_int := to_signed(0,31);
  signal res_0008_0015 : caml_int := to_signed(0,31);
  signal acc_0004_0011 : caml_int := to_signed(0,31);
  signal n_0005_0012 : caml_int := to_signed(0,31);
  signal acc_0004_0013 : caml_int := to_signed(0,31);
  signal n_0005_0014 : caml_int := to_signed(0,31);
  
  type STATE_0023_T is (Q_0022, Q_0019, P_0020, R_0021, AUX_0003_0016, IDLE);
  signal STATE_0023 : STATE_0023_T;
  
  type STATE_0024_T is (IDLE_L_0018, AUX_0003_0017);
  signal STATE_0024 : STATE_0024_T;
  
begin
  process(reset,clk) begin
    if reset = '1' then
      state_0023 <= IDLE;
    elsif rising_edge(clk) then
      case STATE_0023 is
        when Q_0022 =>
          result <= res_0008_0015;
          state_0023 <= IDLE;
        when Q_0019 =>
          start_l_0018 <= '1';
          state_0023 <= P_0020;
        when P_0020 =>
          start_l_0018 <= '0';
          acc_0004_0011 <= to_signed(1,31);
          n_0005_0012 <= a;
          state_0023 <= AUX_0003_0016;
        when R_0021 =>
          if rdy_l_0018 = '1' then
            res_0008_0015 <= tmp_0006_0009 + tmp_0007_0010;
            state_0023 <= Q_0022;
          else
            state_0023 <= R_0021;
          end if;
        when AUX_0003_0016 =>
          if n_0005_0012 <= to_signed(0,31) then
            tmp_0006_0009 <= acc_0004_0011;
            state_0023 <= R_0021;
          else
            acc_0004_0011 <= RESIZE((acc_0004_0011 * n_0005_0012),31);
            n_0005_0012 <= n_0005_0012 - to_signed(1,31);
            state_0023 <= AUX_0003_0016;
          end if;
        when IDLE =>
          if start = '1' then
            rdy <= '0';
            state_0023 <= Q_0019;
          else
            rdy <= '1';
            state_0023 <= IDLE;
          end if;
        end case;
      end if;
    end process;
  process(reset,clk) begin
    if reset = '1' then
      state_0024 <= IDLE_L_0018;
    elsif rising_edge(clk) then
      case STATE_0024 is
        when IDLE_L_0018 =>
          if start_l_0018 = '1' then
            rdy_l_0018 <= '0';
            acc_0004_0013 <= to_signed(1,31);
            n_0005_0014 <= b;
            state_0024 <= AUX_0003_0017;
          else
            rdy_l_0018 <= '1';
            state_0024 <= IDLE_L_0018;
          end if;
        when AUX_0003_0017 =>
          if n_0005_0014 <= to_signed(0,31) then
            tmp_0007_0010 <= acc_0004_0013;
            state_0024 <= IDLE_L_0018;
          else
            acc_0004_0013 <= RESIZE((acc_0004_0013 * n_0005_0014),31);
            n_0005_0014 <= n_0005_0014 - to_signed(1,31);
            state_0024 <= AUX_0003_0017;
          end if;
        end case;
      end if;
    end process;
  
end architecture;
```