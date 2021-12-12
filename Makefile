CAMLC=ocamlc
CAMLLEX=ocamllex
MENHIR=menhir # --explain
CAMLDEP=ocamldep

EXE=compile
PCF_FLAGS=

INCLUDES=-I src -I src/misc\
         -I src/target\
         -I src/esml\
         -I src/translations\
         -I src/frontend\
         -I src/simulation\
         -I src/optimisation

OBJS=src/misc/misc.cmo\
     src/misc/monads.cmo\
     src/misc/loc.cmo\
     src/misc/gensym.cmo\
     \
     src/esml/esml.cmo\
     src/esml/ktypes.cmo\
     src/esml/pp_ktypes.cmo\
     \
     src/frontend/types.cmo\
     src/frontend/ast.cmo\
     \
     src/translations/esml2vhdl.cmo\
     \
     src/target/gen_hw_tcl.cmo\
     src/target/gen_platform.cmo\
     \
     \
     src/esml/pprint_atom.cmo\
     src/esml/pprint_esml.cmo\
     src/frontend/pprint_ast.cmo\
     \
     src/simulation/ast2ocaml.cmo\
     \
     src/frontend/typing.cmo\
     src/frontend/check_tailrec.cmo\
     src/frontend/ast_rename.cmo\
     src/translations/macle2vsml.cmo\
     src/translations/vsml2esml.cmo\
     src/frontend/occur.cmo\
     src/frontend/inline.cmo\
     src/frontend/macro_expansion.cmo\
     \
     src/optimisation/transparent.cmo\
     src/optimisation/propagation.cmo\
     src/optimisation/let_floating.cmo\
     \
     src/frontend/parser.cmo\
     src/frontend/lexer.cmo\
     src/frontend/main.cmo\

    # src/fsmcomp/debug/main_debug.cmo\

SRCS=`find src -name "*.ml*"`
all: prepare src/frontend/parser.cmi $(OBJS)
	$(CAMLC) $(FLAGS) $(INCLUDES) -o $(EXE) $(OBJS)

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi 

.ml.cmo:
	$(CAMLC) $(INCLUDES) $(FLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(INCLUDES) $(FLAGS) -c $<

.mly.ml:
	$(MENHIR) $<

.mll.ml:
	$(CAMLLEX) $<

depend:
	$(CAMLDEP) $(INCLUDES) $(SRCS) > .depend

include .depend

prepare:
	mkdir -p gen/rtl/misc
	mkdir -p gen/c
	mkdir -p gen/ml
	mkdir -p gen/apps


FILE=
OPT=
DST=bench/dst/$(basename $(notdir $(FILE))).vhdl
test:
	./compile $(FILE) > $(DST) ; ghdl -a $(DST)

run:
	echo "-- ./compile $(OPT) $(FILE)\n" > $(FILE).vhdl
	./compile $(OPT) $(FILE) >> $(FILE).vhdl

clean:	clean-cc
	rm -f `find . -name "*.cmo"`
	rm -f `find . -name "*.cmi"`
	rm -f $(EXE)
	
clean-cc:	clean-gen-cc prepare

clean-gen-cc:
	(cd gen; make clean)

check-cc:
	(cd gen; make check)
