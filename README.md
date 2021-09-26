# Macle (automata-based applicative language)

A Tool dedicated to FPGA programming, directly usable in combination with [O2B](https://github.com/jserot/O2B), an implementation of the OCaml Virtual Machine.

### dependencies

- OCaml >= 4.09
- opam install menhir

### Usage
```shell

$ make
$ ./compile bench/simples/identity.ml  bench/simples/combinators.ml 

- process bench/simples/identity.ml:
  info: circuit  "identity"  generated in folder gen/.

- process bench/simples/combinators.ml:
  info: circuit  "sum_list"  generated in folder gen/.
  info: circuit  "sum_array"  generated in folder gen/.
  info: circuit  "sum_array2"  generated in folder gen/.
  info: circuit  "array_map_inc_by4"  generated in folder gen/.
$ make check-cc
(cd gen; make check)
ghdl -a -fno-color-diagnostics rtl/misc/*.vhd rtl/*.vhd;
rm -f *.o *.cf
$
```