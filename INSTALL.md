# install the Macle Compiler 

### install (https://ocaml.org/learn/tutorials/up_and_running.html)

#### install opam

- on Debian:
  ```bash
  $ apt-get install opam
  ```

- on Ubuntu:
  ```bash
  $ add-apt-repository ppa:avsm/ppa
  $ apt update
  $ apt install opam
  ```

- on MacOS:
     ```bash
     $ brew install gpatch
     $ brew install opam
     ```
   or 
   ```bash
   $ port install opam
   ```

- for other systems: see https://opam.ocaml.org/doc/Install.html

#### install the OCaml compiler

- environment setup

```bash
$ opam init
$ eval `opam env`
```
- install given version of the compiler
```bash
opam switch create ocaml-base-compiler.4.09.0
$ $ eval `opam env`
```

#### install the Macle dependencies
  ```bash
  $ opam install menhir
  ```
-----