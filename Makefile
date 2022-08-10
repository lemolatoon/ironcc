CC=clang
CFLAGS=-g3 -static
MAKEFILE_DIR:=$(dir $(abspath $(lastword $(MAKEFILE_LIST))))
COMPILER=$(MAKEFILE_DIR)/target/debug/ironcc

$(COMPILER): FORCE
	cargo build

tmp.s: tmp.c $(COMPILER)
	cargo run tmp.c

a.out: tmp.s link.c
	$(CC) tmp.s tmp_linked.c -o a.out $(CFLAGS)

run: a.out 
	./a.out


test: $(COMPILER)
	./test/test.sh

testc: $(COMPILER) test/test.c
	cd test && \
	$(COMPILER) test.c && \
	$(CC) test.s test_utils.c $(CFLAGS) -o tmp && \
	./tmp

prpred_life_game.s: $(COMPILER) life_game.c
	gcc -g3 -E life_game.c > prpred_life_game.c
	cargo run prpred_life_game.c

life: prpred_life_game.s utils.c
	clang -g3 prpred_life_game.s utils.c
	./a.out

	


cargo_test: FORCE
	cargo test

testall:  cargo_test test testc


.PHONY: FORCE test cargo_test testall run testc
