CC=clang
CFLAGS=-g -static
COMPILER=target/debug/ironcc

$(COMPILER): FORCE
	cargo build

tmp.s: tmp.c
	cargo run tmp.c

tmp: tmp.s
	$(CC) tmp.s -o tmp $(CFLAGS)

test: $(COMPILER)
	./test/test.sh

cargo_test: FORCE
	cargo test

testall: test cargo_test


.PHONY: FORCE test cargo_test testall
