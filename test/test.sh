#!/bin/bash
SCRIPT_DIR=$(cd $(dirname $0); pwd)
cd $SCRIPT_DIR
COMPILER="$SCRIPT_DIR/../target/debug/ironcc"

call () {
    input="$1"

    echo "$input" > tmp.c
    $COMPILER tmp.c

    clang -o tmp tmp.s
    ./tmp
}

assert() {
    expected="$1"
    input="$2"

    call "$input"
    actual="$?"

    if [ "$actual" = "$expected" ]; then
        echo "$input => $actual"
    else
        echo "$input => $expected expected, but got $actual"
        exit 1
    fi
}

assert 1 "1"
assert 0 "0"
assert 255 "255"

assert 2 "1 + 1"
assert 2 "3 - 1"
assert 96 "0 - 1   + 99 -2"

assert 4 "1 * 2 + 8 / 4"
assert 6 "1 * 2 + 2 *8 / 4"
assert 97 "1 * 2 - 2 *8 / 4 + 99"
assert 99 "1 * (2 - 2) *8 / 4 + 99"
assert 5 "(1 - 2) * (0 - 8) - 3*1"

assert 10 "-10 + 20"
assert 30 "-(-10) + 20"
assert 20 "+(-10) + 30"

assert 1 "1 == 1"
assert 0 "1 != 1"
assert 0 "1 > 2"
assert 1 "1 < 2"
assert 1 "1 <= 2"
assert 1 "1 <= 1"
assert 0 "1 >= 2"
assert 1 "1 >= 1"

assert 1 "1 == 1"
assert 0 "1 != 1"
assert 0 "(1 == 1) < (1 != 1)"
assert 0 "1 == 1 < 1 != 1"
assert 1 "2 >= 2"

assert 100 "((32 + -980) <= (-  5*4 * (-2) + 9 -997 * (-2) / (-2))) + 99"


echo "OK"