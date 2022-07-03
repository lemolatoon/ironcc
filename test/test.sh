#!/bin/bash
SCRIPT_DIR=$(cd $(dirname $0); pwd)
cd $SCRIPT_DIR
COMPILER="$SCRIPT_DIR/../target/debug/ironcc"
mkdir ../tmp -p


unique() {
  echo $RANDOM | md5sum | head -c 10
}

LOG=../tmp/log$(unique)


call () {
    input="$1"

    UNIQ=$(unique)

    echo "$input" > ../tmp/tmp$UNIQ.c && \
    cd ../tmp && \
    $COMPILER tmp$UNIQ.c && \
    cd $SCRIPT_DIR && \
    clang -o ../tmp/tmp$UNIQ ../tmp/tmp$UNIQ.s && \
    ../tmp/tmp$UNIQ
}

_assert() {
    expected="$1"
    input="$2"

    call "$input"
    actual="$?"

    if [ "$actual" = "$expected" ]; then
        echo "$input => $actual"
    else
        printf "\033[31m%s => %s expected, but got %s\033[0m\n" "$input" "$expected" "$actual" | tee $LOG -a
        exit 1
    fi
}

assert() {
    _assert "$1" "$2" &
}

clean() {
    rm ../tmp/*
}


assert 1 "1;"
assert 0 "0;"
assert 255 "255;"

assert 2 "1 + 1;"
assert 2 "3 - 1;"
assert 96 "0 - 1   + 99 -2;"

assert 4 "1 * 2 + 8 / 4;"
assert 6 "1 * 2 + 2 *8 / 4;"
assert 97 "1 * 2 - 2 *8 / 4 + 99;"
assert 99 "1 * (2 - 2) *8 / 4 + 99;"
assert 5 "(1 - 2) * (0 - 8) - 3*1;"

assert 10 "-10 + 20;"
assert 30 "-(-10) + 20;"
assert 20 "+(-10) + 30;"

assert 1 "1 == 1;"
assert 0 "1 != 1;"
assert 0 "1 > 2;"
assert 1 "1 < 2;"
assert 1 "1 <= 2;"
assert 1 "1 <= 1;"
assert 0 "1 >= 2;"
assert 1 "1 >= 1;"

assert 1 "1 == 1;"
assert 0 "1 != 1;"
assert 0 "(1 == 1) < (1 != 1);"
assert 1 "1 == 1 < 1 != 1;"
assert 1 "2 >= 2;"

assert 100 "((32 + -980) <= (-  5*4 * (-2) + 9 -997 * (-2) / (-2))) + 99;"

assert 3 "1 + 1; 1 + 2;"
assert 5 "a = 5; a;"
assert 55 "a = 1; b = 2; c = 3; z = 4; y = 5; x = 6; t = 10; u = 9; v = 8; w = 7; a +  b + c + z + y + x + t + u + v + w;"
assert 3 "abc = 22; cde=7; abc / cde;"
assert 1 "abc = 22; cde=7; return abc > cde;"
assert 4 "return 4; return 5;"

assert 3 " a = 1; if (44 > 32) a = 3; if(44 < 32) a = 5; return a;"
assert 100 " a = 5; if (55 != 43) a = 100; else a = 50; return a;"

assert 1 "i = 0; sum = 1; while(i <= 10) i = 11; return sum;"
assert 64 "i = 1; while(i <=55) i = i*2; return i;"

assert 1 "10 <= 10;"

assert 55 " sum = 0; for (i = 1; i <= 10; i = i + 1) sum = sum + i; return sum;"
assert 1 " sum = 0; for (i = 1; i == 1; i = i + 1) sum = sum + i; return sum;"

# remains
assert 1 "6 % 3 == 0;"
assert 3 "6 / 2 % (3 + 1);"
assert 4 "{a = 6 / 2 % (3 + 1); return a + 1;}"

assert 0 "103 % 102 == 0;"
assert 1 "n = 103; i = 102; result = 1; if (n % i == 0) {result = 0;} return result;"
assert 102 "v =0; for (i = 2; i < 103; i = i + 1) {v = i;} return v;"
assert 1 " n = 103;  result = 1; for (i = 2; i < 103; i = i + 1) {if (n % i == 0) {result = 0;}} return result;"

assert 55 " {sum = 0; i = 1; while(i <= 10) {sum = sum + i; i = i + 1;} return sum;}"
assert 102 "   { sum  = 5;i  = 10;while (i * 2 <= 90000){if ((i + 5) % 4 == 0){i = i + 2;sum = sum + i + 2;}i = i * 2;sum = sum + i + 9;}return sum % 256;}"

assert 10 "a  = 1; for (i = 0; i < 2; i = i + 1) {for (j = 0; j < 3; j = j + 1) {a = a + i + j;}} return a;"

assert 105 "{ sum = 4; i = 0; while (i < 74) {for (j = 1; j < 10; j = j + 1) { sum = sum + j;}i = i + 3;} return sum % 256;}"


wait
clean

echo "OK"