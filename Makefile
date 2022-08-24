CC=gcc
CFLAGS=-g3 -static
MAKEFILE_DIR:=$(dir $(abspath $(lastword $(MAKEFILE_LIST))))
COMPILER=$(MAKEFILE_DIR)/target/debug/ironcc

$(COMPILER): FORCE
	cargo build

tmp.s: tmp.c $(COMPILER)
	cargo run tmp.c

a.out: tmp.s link.c
	$(CC) tmp.s tmp_linked.c -o a.out $(CFLAGS)

run: a.out $(COMPILER)
	./a.out


test: $(COMPILER)
	./test/test.sh

testc: $(COMPILER) test/test.c
	cd test && \
	$(COMPILER) test.c && \
	$(CC) test.s test_utils.c $(CFLAGS) -o tmp && \
	./tmp

life_game.s: $(COMPILER) life_game.c
	cargo run life_game.c

life: life_game.s
	clang -g3 life_game.s
	./a.out

life2: samples/cellular_automaton.c
	cargo run samples/cellular_automaton.c
	clang cellular_automaton.s
	./a.out

donut: rotate.c
	cargo run rotate.c
	clang rotate.s
	./a.out

tetris.s: $(COMPILER) tetris.c
	gcc -E tetris.c > prpr_tetris.c
	cargo run prpr_tetris.c
	mv prpr_tetris.s tetris.s

tetris: tetris.s
	clang -g3 tetris.s
	./a.out
	
clang_test: test/test.c test/test_utils.c
	clang test/test.c test/test_utils.c
	./a.out

cargo_test: FORCE
	cargo test

testall:  cargo_test test testc


.PHONY: FORCE test cargo_test testall run testc
