CC=clang
CFLAGS=-g3 -static
COMPILER=target/debug/ironcc

$(COMPILER): FORCE
	cargo build

tmp.s: tmp.c $(COMPILER)
	cargo run tmp.c

a.out: tmp.s
	$(CC) tmp.s -o a.out $(CFLAGS)

run: a.out 
	./a.out


test: $(COMPILER)
	./test/test.sh

cargo_test: FORCE
	cargo test

testall: test cargo_test


.PHONY: FORCE test cargo_test testall run
