CC=clang
CFLAGS=-g3 -static
COMPILER=target/debug/ironcc

$(COMPILER): FORCE
	cargo build

tmp.s: tmp.c $(COMPILER)
	cargo run tmp.c

a.out: tmp.s link.c
	$(CC) tmp.s link.c -o a.out $(CFLAGS)

run: a.out 
	./a.out


test: $(COMPILER)
	./test/test.sh

cargo_test: FORCE
	cargo test

testall:  cargo_test test


.PHONY: FORCE test cargo_test testall run
