CC=clang
CFLAGS=-g3 -static
MAKEFILE_DIR:=$(dir $(abspath $(lastword $(MAKEFILE_LIST))))
COMPILER=$(MAKEFILE_DIR)/target/debug/ironcc
LEMOLA_CC_PATH=$(MAKEFILE_DIR)validation/lemola_cc

$(COMPILER): FORCE
	cargo build 

tmp.s: tmp.c $(COMPILER)
	cargo run tmp.c

a.out: tmp.s link.c
	$(CC) tmp.s -o a.out $(CFLAGS)

run: a.out $(COMPILER)
	./a.out

fmt: FORCE
	cargo fmt
	clang-format -i */**/*.c

test: $(COMPILER)
	./test/test.sh

testc: $(COMPILER) test/test.c
	$(COMPILER) test/test.c && \
	$(CC) test.s test/test_utils.c $(CFLAGS) -o a.out && \
	./a.out

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

main.s: $(LEMOLA_CC_PATH)/src/main.c $(COMPILER)
	$(COMPILER) $(LEMOLA_CC_PATH)/src/main.c
tokenizer.s: $(LEMOLA_CC_PATH)/src/tokenizer.c $(COMPILER)
	$(COMPILER) $(LEMOLA_CC_PATH)/src/tokenizer.c
parser.s: $(LEMOLA_CC_PATH)/src/parser.c $(COMPILER)
	$(COMPILER) $(LEMOLA_CC_PATH)/src/parser.c
code_gen.s: $(LEMOLA_CC_PATH)/src/code_gen.c $(COMPILER)
	$(COMPILER) $(LEMOLA_CC_PATH)/src/code_gen.c
utils.s: $(LEMOLA_CC_PATH)/src/utils.c $(COMPILER)
	$(COMPILER) $(LEMOLA_CC_PATH)/src/utils.c

main.o: main.s
	$(CC) main.s -c -o main.o
tokenizer.o: tokenizer.s
	$(CC) tokenizer.s -c -o tokenizer.o
parser.o: parser.s
	$(CC) parser.s -c -o parser.o
code_gen.o: code_gen.s
	$(CC) code_gen.s -c -o code_gen.o
utils.o: utils.s
	$(CC) utils.s -c -o utils.o

lemola_cc: main.o tokenizer.o parser.o code_gen.o utils.o
	$(CC) $(CFLAGS) main.o tokenizer.o parser.o code_gen.o utils.o -o lemola_cc

lemola_cc_test: lemola_cc $(LEMOLA_CC_PATH)/test/test.c $(LEMOLA_CC_PATH)/test/test_utils.c
	cd $(LEMOLA_CC_PATH)/test && \
		$(MAKEFILE_DIR)/lemola_cc test.c && \
		$(CC)  -c test_utils.c -g3 && \
		$(CC)  src.s test_utils.o -o tmp -g3 && \
		./tmp

testall:  cargo_test test testc


.PHONY: FORCE test cargo_test testall run testc
