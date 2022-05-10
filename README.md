# Macle (ML accelerator)

A language to program hardware-accelerated functions in ML style,
and call them from OCaml program running on FPGA via O2B (https://github.com/jserot/O2B).

More generally, Macle Compiler produces complete O2B platforms (including VHDL, C, OCaml code and scripts) 
to configure an Intel FPGA (with Quartus 15.1 and latter).

### install 

(see INSTALL.md)

#### use Macle

```
$ make
$ ./maclec tests/eval_exp.ml 
  info: circuit  "eval_exp"  generated in folder gen/.

$ ./maclec tests/eval_exp.ml -simul

   # translate the source Macle/OCaml program into an OCaml one.

$ ./maclec tests/eval_exp.ml -simul

   # translate the intermediate representation used by the compiler into an OCaml program. 
```