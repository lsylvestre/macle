CAMLC=ocamlc
CAMLLEX=ocamllex
MENHIR=menhir # --explain
CAMLDEP=ocamldep

EXE=compile
PCF_FLAGS=

INCLUDES=-I src -I src/misc -I src/fsmcomp\
         -I src/fsmcomp/target -I src/fsmcomp/kernel\
         -I src/fsmcomp/debug\
         -I src/fsmcomp/renaming -I src/fsmcomp/translations\
         -I src/frontend

OBJS=src/misc/misc.cmo\
     src/misc/monads.cmo\
     src/misc/loc.cmo\
     src/misc/gensym.cmo\
     \
     src/fsmcomp/kernel/ktypes.cmo\
     src/fsmcomp/kernel/atom.cmo\
     src/fsmcomp/kernel/kast.cmo\
     src/fsmcomp/kernel/pprint_kast.cmo\
     \
     src/fsmcomp/translations/esml2vhdl.cmo\
     src/fsmcomp/translations/psml2esml.cmo\
     src/fsmcomp/translations/vsml2psml.cmo\
     \
     src/fsmcomp/target/gen_hw_tcl.cmo\
     src/fsmcomp/target/gen_platform.cmo\
     \
     src/fsmcomp/renaming/psml_rename.cmo\
     src/fsmcomp/renaming/vsml_rename.cmo\
     src/fsmcomp/renaming/vsml_states_rename.cmo\
     \
     \
     src/frontend/types.cmo\
     src/frontend/ast.cmo\
     src/frontend/pprint_ast.cmo\
     src/frontend/ast2ocaml.cmo\
     \
     src/frontend/typing.cmo\
     src/frontend/ast_rename.cmo\
     src/frontend/ast2kast.cmo\
     src/frontend/inline.cmo\
     src/frontend/propagation.cmo\
     \
     src/fsmcomp/debug/parser_vsml.cmo\
     src/fsmcomp/debug/lexer_vsml.cmo\
     \
     src/frontend/parser.cmo\
     src/frontend/lexer.cmo\
     src/frontend/main.cmo\

    # src/fsmcomp/debug/main_debug.cmo\

SRCS=`find src -name "*.ml*"`
all: prepare src/fsmcomp/debug/parser_vsml.cmi src/frontend/parser.cmi $(OBJS)
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
	
clean-cc:
	(cd gen; make clean)

check-cc:
	(cd gen; make check)
