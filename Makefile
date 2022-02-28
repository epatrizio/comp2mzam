VM=./mini-zam/vm/minizam
EXE=c2mz

all:
	ocamlc -c utils.ml;
	ocamllex lexer.mll;
	ocamlc -c ast.ml;
	ocamlc -c compiler.ml;
	menhir parser.mly;
	ocamlc -c parser.mli;
	ocamlc -c parser.ml;
	ocamlc -c lexer.ml;
	ocamlc -c main.ml;
	ocamlc -o $(EXE) utils.cmo ast.cmo compiler.cmo lexer.cmo parser.cmo main.cmo

clean:
	rm -rf *.cmo *.cmi lexer.ml parser.ml parser.mli $(EXE)

clean_bc:
	rm -rf tests/build/bc_*.txt

compile:
	./$(EXE) $(SC)

vm:
	$(VM) $(BC)

vm_demo:
	$(VM) tests/build/demo-bytecode.txt
