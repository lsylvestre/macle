# Macle (*ML accelerator*)

A language to program hardware-accelerated functions in ML style,
and call them from OCaml programs running on FPGA via the O2B (https://github.com/jserot/O2B).

O2B is an implementation of the OCaml virtual machine targeting a softcore processor designed on a FPGA.
O2B supports currently the Intel Nios II softcore processor on the Terrasic DE10-Lite Board (Intel Max 10 FPGA)
with 50K logic elements, 1.6 MB of onchip memory, 64 MB of external memory (SDRAM) and a frequency of 50 Mhz.

Macle Compiler produces complete O2B platforms (including VHDL, C, OCaml code and scripts) 
to configure an Intel FPGA (with Quartus 15.1 and latter).

### install 

(see INSTALL.md)

#### use Macle

```
$ make
$ ./maclec examples/eval_exp.ml
  info: circuit  "eval_exp"  generated in folder gen/.

$ ./maclec examples/eval_exp.ml -simul

   # translates the Macle/OCaml source program 
     into an OCaml one.

$ ./maclec examples/eval_exp.ml -ocaml-backend

   # translates in OCaml the intermediate representation of the Macle/OCaml source program.

$ make run SRC=examples/eval_exp.ml

   # translates in OCaml the intermediate representation of the Macle/OCaml source program
     and run it in the OCaml toplevel.
```

##### measure execution time

```
$ ./maclec -time exemple/gcd.ml
```

##### increase the frequency of the Nios II softcore processor by a multiplicative factor 2 or 3


```
$ ./maclec -nios2-freq-multiplier 3  exemple/gcd.ml
```


##### Use external memory (SDRAM) by default, or onchip memory with the -onchip option

```
$ ./maclec -onchip examples/matrix_product.ml 
```
