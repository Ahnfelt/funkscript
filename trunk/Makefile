PACKS = extlib
SOURCES = ColoTypes.ml ColoParser.mly ColoLexer.mll \
ColoVariant.ml ColoOcaml.ml ColoFormat.ml ColoInterpreter.ml
RESULT = colo
MAKEFILE = OCamlMakefile

all: byte-code

run: all
	./$(RESULT) && make -f RunMakefile && ./test

include $(MAKEFILE)

