# Force this target to run every time
.PHONY: all clean

all:
	@(cd build && rm -rf *.cmi *.cmx *.o *language)
	@(cd src && ocamlfind ocamlopt -package ppx_deriving.show -linkpkg -o language util.ml testing.ml lexer.ml parser.ml evaluator.ml main.ml) || \
	(cd src && rm -rf *.cmi *.cmx *.o *language && exit 1)
	@(cd src && mv *.cmi *.o *.cmx *language ../build/)
	@./build/language