CAMLC=ocamlc
CAMLLEX=ocamllex
MENHIR=menhir
CAMLDEP=ocamldep

EXE=maclec

INCLUDES=-I src -I src/misc\
         -I src/target\
         -I src/esml\
         -I src/translations\
         -I src/macle\
         -I src/middle_end\
         -I src/simulation\
         -I src/optimisation

OBJS=src/misc/misc.cmo\
     src/misc/monads.cmo\
     src/misc/err.cmo\
     src/misc/loc.cmo\
     src/misc/gensym.cmo\
     \
     src/esml/esml.cmo\
     \
     src/macle/types.cmo\
     src/macle/ast.cmo\
     \
     src/translations/esml2vhdl.cmo\
     \
     src/target/gen_hw_tcl.cmo\
     src/target/gen_platform.cmo\
     \
     \
     src/esml/pprint_esml.cmo\
     src/macle/pprint_ast.cmo\
     \
     src/simulation/ast2ocaml.cmo\
     \
     src/macle/typing.cmo\
     src/macle/ast_mapper.cmo\
     src/macle/check_tailrec.cmo\
     \
     src/middle_end/ast_rename.cmo\
     \
     src/middle_end/is_atom.cmo\
     src/middle_end/derecursivation.cmo\
     src/translations/anf.cmo\
     src/translations/vsml2esml.cmo\
     \
     src/middle_end/occur.cmo\
     src/middle_end/inline.cmo\
     src/middle_end/inlining.cmo\
     src/middle_end/macro_expansion.cmo\
     src/middle_end/safe_array_access.cmo\
     \
     src/optimisation/transparent.cmo\
     src/optimisation/equations.cmo\
     src/optimisation/propagation.cmo\
     src/optimisation/let_floating.cmo\
     \
     src/macle/parser.cmo\
     src/macle/lexer.cmo\
     src/macle/main.cmo

SRCS=`find src -name "*.ml*"`
all: prepare src/macle/parser.cmi $(OBJS)
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

prepare:	gen
	mkdir -p gen/rtl/misc
	mkdir -p gen/c
	mkdir -p gen/ml
	mkdir -p gen/apps

clean:	clean-cc
	rm -f `find . -name "*.cmo"`
	rm -f `find . -name "*.cmi"`
	rm -f $(EXE)

clean-cc:	clean-gen-cc prepare

clean-gen-cc:
	(cd gen; make clean)

check-cc:
	(cd gen; make check)

run:
	(./$(EXE) $(SRC) -ocaml-backend | ocaml -dsource)

compile:
	./$(EXE) $(SRC)