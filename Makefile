CC=clang
COMPILER=target/debug/ironcc

$(COMPILER): FORCE
	cargo build

tmp.s:
	cargo run tmp.c

tmp: tmp.s
	clang tmp.s -o tmp

test: $(COMPILER)
	./test/test.sh


.PHONY: FORCE test