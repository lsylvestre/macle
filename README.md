# Macle (automata-based applicative language)

A Tool dedicated to FPGA programming, directly usable in combination with [O2B](https://github.com/jserot/O2B), an implementation of the OCaml Virtual Machine.

### dependencies

- OCaml >= 4.09
- opam install menhir

### Macle Syntax

```
pi ::= ci_1 ;; ... ci_n ;;;[;]^* P   (where P is an OCaml program)
ci ::= circuit f x1 ... xn = e 
e ::= c | x | f e1 ... en | let rec f x1 ... xn = e and ... in e
    | if e1 then e2 else e3
    | let x = e and ... in e
    | <unop> e
    | e1 <binop> e2
    | match e with h1 | ... hn

h ::= C(p1, ... pn) -> e

p ::= _ | x
```
- exemple (*bench/simples/fact.ml*) 

```ocaml
(* Macle circuit *)

circuit fact n = 
  let rec aux acc n = 
    if n <= 0 then acc else aux (acc*n) (n-1) in 
  aux 1 n

;;; 

(* OCaml program *)

print_int @@ fact 6;;
```

### Usage

```
$ make
```

- simulation (translation into OCaml)

```ocaml
$ ./compile bench/simples/fact.ml -simul
let fact (n : int) =
  (let rec aux (acc : int)
     (n : int) =
     (if (n <= 0)
     then acc
     else (aux (acc * n) (n - 1)))
     in (aux 1 n)
   )
  ;;
  
 

(* OCaml program *)

print_int @@ fact 6;;

$
```

- code generation (VHDL)

```vhdl
$ ./compile bench/simples/fact.ml -vhdl-only
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
  signal acc_0x5 : caml_int := to_signed(0,31);
  signal n_0x6 : caml_int := to_signed(0,31);
  
  type STATE_0x8_T is (IDLE, AUX_0x7);
  signal STATE_0x8 : STATE_0x8_T;
  
begin
  process(reset,clk) begin
    if reset = '1' then
      state_0x8 <= IDLE;
    elsif rising_edge(clk) then
      case STATE_0x8 is
        when IDLE =>
          if start = '1' then
            rdy <= '0';
            acc_0x5 <= to_signed(1,31);
            n_0x6 <= n;
            state_0x8 <= AUX_0x7;
          else
            rdy <= '1';
            state_0x8 <= IDLE;
          end if;
        when AUX_0x7 =>
          if n_0x6 <= to_signed(0,31) then
            result <= acc_0x5;
            state_0x8 <= IDLE;
          else
            acc_0x5 <= RESIZE((acc_0x5 * n_0x6),31);
            n_0x6 <= n_0x6 - to_signed(1,31);
            state_0x8 <= AUX_0x7;
          end if;
        end case;
      end if;
    end process;
  
end architecture;
$
```

- **O2B** platform generation

```
./compile bench/simples/fact.ml
  info: circuit  "fact"  generated in folder gen/.
```
