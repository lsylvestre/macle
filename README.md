# Macle (automata-based applicative language)

A Tool dedicated to FPGA programming, directly usable in combination with [O2B](https://github.com/jserot/O2B), an implementation of the OCaml Virtual Machine.

### dependencies

- OCaml >= 4.09
- opam install menhir

### Macle Syntax

```
pi ::= ci_1 ;; ... ci_n ;;;[;]^* P   (where P is an OCaml program)
ci ::= circuit x(x1,... xn) = e 
e ::= c | x | x(e1,... en) | let rec f(x1,...xn) = e and ... in e
    | if e1 then e2 else e3
    | let x = e and ... in e
    | <unop> e
    | e1 <binop> e2
```
- exemple (*bench/simples/fact.ml*) 

```ocaml
(* Macle circuit *)

circuit fact(n) = 
  let rec aux(acc,n) = 
    if n <= 0 then acc else aux(acc*n,n-1) in 
  aux(1,n)

;;; 

(* OCaml program *)

print_int @@ fact 6;;
```

### Usage

```shell
$ make
```

- simulation (translation into OCaml)

```shell
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

```shell
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
  signal acc_0002_0004 : caml_int := to_signed(0,31);
  signal n_0003_0005 : caml_int := to_signed(0,31);
  
  type STATE_0007_T is (IDLE, AUX_0001_0006);
  signal STATE_0007 : STATE_0007_T;
  
begin
  process(reset,clk) begin
    if reset = '1' then
      state_0007 <= IDLE;
    elsif rising_edge(clk) then
      case STATE_0007 is
        when IDLE =>
          if start = '1' then
            rdy <= '0';
            acc_0002_0004 <= to_signed(1,31);
            n_0003_0005 <= n;
            state_0007 <= AUX_0001_0006;
          else
            rdy <= '1';
            state_0007 <= IDLE;
          end if;
        when AUX_0001_0006 =>
          if n_0003_0005 <= to_signed(0,31) then
            result <= acc_0002_0004;
            state_0007 <= IDLE;
          else
            acc_0002_0004 <= RESIZE((acc_0002_0004 * n_0003_0005),31);
            n_0003_0005 <= n_0003_0005 - to_signed(1,31);
            state_0007 <= AUX_0001_0006;
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
