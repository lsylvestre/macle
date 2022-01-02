# Macle (automata-based applicative language)

A Tool dedicated to FPGA programming, directly usable in combination with [O2B](https://github.com/jserot/O2B), an implementation of the OCaml Virtual Machine.

### dependencies

- OCaml >= 4.09
- opam install menhir

### Macle Syntax

```
pi ::= d_1 ;; ... d_n ;;;[;]^* P   (where P is an OCaml program)

d ::= <typdef> | ci

<typdef> ::= type t = C1 of ty1_1 * ... ty1_n | Ck of tyk_1 * ... tyk_n 

ci ::= circuit f x1 ... xn = e 
e ::= c 
    | x 
    | f e1 ... en 
    | let rec f x1 ... xn = e and ... in e
    | if e1 then e2 else e3
    | let x = e and ... in e
    | <unop> e
    | e1 <binop> e2
    | match e with h1 | ... hn
    | <ext>

h ::= C(p1, ... pn) -> e

p ::= _ | x

<ext> ::= raise "<message>"
        | !e
        | e1 := e2
        | e1.(e2)
        | e1.(e2) <- e3
        | array_length e
        | map f e
        | reduce f e0 e
        | of_array n e
        | array_reduce_by n f e0 e
        | array_iter_by n f e
```
- exemple (*examples/simples/fact.ml*) 

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
$ ./compile examples/simples/fact.ml -simul
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
$ ./compile examples/simples/fact.ml -vhdl-only
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
  signal acc_0xf : caml_int;
  signal n_0x10 : caml_int;
  
  type STATE_0x11_T is (AUX_0xE, IDLE);
  signal STATE_0x11 : STATE_0x11_T;
  
begin
  process(reset,clk) begin
    if reset = '1' then
      acc_0xf <= to_signed(0,31);
      n_0x10 <= to_signed(0,31);
      rdy <= '1';
      result <= to_signed(0,31);
      state_0x11 <= IDLE;
    elsif rising_edge(clk) then
      case STATE_0x11 is
        when AUX_0xE =>
          if n_0x10 <= to_signed(0,31) then
            result <= acc_0xf;
            state_0x11 <= IDLE;
          else
            acc_0xf <= RESIZE((acc_0xf * n_0x10),31);
            n_0x10 <= n_0x10 - to_signed(1,31);
            state_0x11 <= AUX_0xE;
          end if;
        when IDLE =>
          if start = '1' then
            rdy <= '0';
            acc_0xf <= to_signed(1,31);
            n_0x10 <= n;
            state_0x11 <= AUX_0xE;
          else
            rdy <= '1';
            state_0x11 <= IDLE;
          end if;
        end case;
      end if;
    end process;
  
end architecture;
$
```

- **O2B** platform generation

```
./compile examples/simples/fact.ml
  info: circuit  "fact"  generated in folder gen/.
```
