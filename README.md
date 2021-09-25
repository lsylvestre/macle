# Macle (AutoMAta-based AppliCative LanguagE)

A Tool dedicated to FPGA programming, directly usable in combination with [O2B](https://github.com/jserot/O2B), an implementation of the OCaml Virtual Machine.

### dependencies

- OCaml >= 4.09
- opam install menhir

### Usage
```
$ make
$ ./compile bench/simples/sum_list.ml 
info: circuit  "sum"  generated in folder gen/.
$ make check-cc
(cd gen; make check)
ghdl -a -fno-color-diagnostics rtl/misc/*.vhd rtl/*.vhd;
rm -f *.o *.cf
$
```