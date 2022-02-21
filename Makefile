VM=./mini-zam/vm/minizam
EXE=c2mz

all:
	ocamllex lexer.mll;
	ocamlc -c ast.ml;
	ocamlc -c compiler.ml;
	menhir parser.mly;
	ocamlc -c parser.mli;
	ocamlc -c parser.ml;
	ocamlc -c lexer.ml;
	ocamlc -c main.ml;
	ocamlc -o $(EXE) ast.cmo compiler.cmo lexer.cmo parser.cmo main.cmo

clean:
	rm -rf *.cmo *.cmi $(EXE)

run_demo:
	$(VM) tests/build/demo-bytecode.txt
