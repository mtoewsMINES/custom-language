# Force this target to run every time
.PHONY: all clean

all:
	@(cd build && rm -rf *.cmi *.cmx *.o *language)
	@(cd src && ocamlopt -o language util.ml lexer.ml parser.ml evaluator.ml main.ml) || \
	(cd src && rm -rf *.cmi *.cmx *.o *language && exit 1)
	@(cd src && mv *.cmi *.o *.cmx *language ../build/)
	@./build/language