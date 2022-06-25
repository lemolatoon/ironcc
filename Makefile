CC=clang
CFLAGS=-g -static
COMPILER=target/debug/ironcc

$(COMPILER): FORCE
	cargo build

tmp.s:
	cargo run tmp.c

tmp: tmp.s
	$(CC) tmp.s -o tmp $(CFLAGS)

test: $(COMPILER)
	./test/test.sh


.PHONY: FORCE test