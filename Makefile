VM=./mini-zam/vm/minizam
VM_DEBUG=./mini-zam/vm/minizam-debug
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

check_grammar:
	menhir --list-errors parser.mly

compile:
	@./$(EXE) tests/$(S)

vm:
	@$(VM) tests/build/bc_$(S)

vm_debug:
	@$(VM_DEBUG) tests/build/bc_$(S)

cvm: compile vm

cvm_debug: compile vm_debug

vm_demo:
	$(VM) tests/build/demo-bytecode.txt
