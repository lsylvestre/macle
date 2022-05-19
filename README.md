# Macle (*ML accelerator*)

A language to program hardware-accelerated functions in ML style,
and call them from OCaml program running on FPGA via O2B (https://github.com/jserot/O2B).

More generally, Macle Compiler produces complete O2B platforms (including VHDL, C, OCaml code and scripts) 
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