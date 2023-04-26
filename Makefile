VM=./mini-zam/vm/minizam
VM_DEBUG=./mini-zam/vm/minizam-debug
EXE=c2mz
RED=\033[0;31m
GREEN=\033[0;32m
NC=\033[0m # No Color

all:
	ocamlc -c ast.ml;
	ocamlc -c utils.ml;
	ocamllex lexer.mll;
	ocamlc -c compiler.ml;
	menhir parser.mly;
	ocamlc -c parser.mli;
	ocamlc -c parser.ml;
	ocamlc -c lexer.ml;
	ocamlc -c typer.ml;
	ocamlc -c domain.ml;
	ocamlc -c domain_value.ml;
	ocamlc -c domain_non_rel.ml;
	ocamlc -c domain_concrete.ml;
	ocamlc -c domain_constant.ml;
	ocamlc -c abstract_interpreter.ml;
	ocamlc -c main.ml;
	ocamlc -o $(EXE) utils.cmo ast.cmo compiler.cmo lexer.cmo parser.cmo typer.cmo \
	domain.cmo domain_value.cmo domain_non_rel.cmo domain_concrete.cmo domain_constant.cmo \
	abstract_interpreter.cmo main.cmo

clean:
	rm -rf *.cmo *.cmi lexer.ml parser.ml parser.mli $(EXE)

clean_bc:
	rm -rf tests/build/bc_*.txt

check_grammar:
	menhir --list-errors parser.mly

help:
	@./$(EXE) --help

compile:
	@./$(EXE) tests/$(S)

compile_debug:
	@./$(EXE) --debug tests/$(S)

compile_no_typing:
	@./$(EXE) --no-typing tests/$(S)

abs_inter_concrete:
	@./$(EXE) --abs-inter-concrete tests/$(S)

abs_inter_constant:
	@./$(EXE) --abs-inter-constant tests/$(S)

vm:
	@$(VM) tests/build/bc_$(S)

vm_debug:
	@$(VM_DEBUG) tests/build/bc_$(S)

cvm: compile vm

cvm_no_typing: compile_no_typing vm

cvm_debug: compile vm_debug

cvm_debug_no_typing: compile_no_typing vm_debug

test:
	$(eval RES := $(shell S=$(S) make cvm))
	@if [ $(RES) = ${R} ]; then echo "$(GREEN)$(S) PASSED$(NC)"; else echo "$(RED)$(S) FAILED $(RES)<>$(R)$(NC)"; fi

# KO > todo
tests_suite: clean_bc
	$(MAKE) test R=5 S=t0.txt

vm_demo:
	$(VM) tests/build/demo-bytecode.txt
