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

echo "OK"