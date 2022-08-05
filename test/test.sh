#!/bin/bash
SCRIPT_DIR=$(cd $(dirname $0); pwd)
cd $SCRIPT_DIR
COMPILER="$SCRIPT_DIR/../target/debug/ironcc"
mkdir ../tmp -p
CC=clang

result=0

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
    $CC -o ../tmp/tmp$UNIQ link.c ../tmp/tmp$UNIQ.s && \
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
        result=1
        exit 1
    fi
}


assert() {
    _assert "$1" "$2" &
}

clean() {
    rm ../tmp/*
}

check() {
 echo -n "TEST STATUS: "
    if [ ! -f $LOG ]; then
        printf "\033[32mPASSED\033[0m\n"
        return 0
    else
        N_FAILED=$(cat $LOG | wc -l)
        printf "\033[31mFAILED\033[0m\t"
        printf  "\033[31m%d\033[0m tests failed.\n\n" $N_FAILED
        return 1
    fi

}


assert 1 "int main(){return 1;}"
assert 0 "int main(){return 0;}"
assert 255 "int main(){return 255;}"

assert 2 "int main(){return 1 + 1;}"
assert 2 "int main(){return 3 - 1;}"
assert 96 "int main(){return 0 - 1   + 99 -2;}"


assert 4 "int main(){return 1 * 2 + 8 / 4;}"
assert 6 "int main() {return 1 * 2 + 2 *8 / 4;}"
assert 97 "int main() {return 1 * 2 - 2 *8 / 4 + 99;}"
assert 99 "int main() {return 1 * (2 - 2) *8 / 4 + 99;}"
assert 5 "int main() {return (1 - 2) * (0 - 8) - 3*1;}"

assert 10 "int main(){return -10 + 20;}"
assert 30 "int main() {return -(-10) + 20;}"
assert 20 "int main() {return +(-10) + 30;}"

assert 1 "int main(){return 1 == 1;}"
assert 0 "int main(){return 1 != 1;}"
assert 0 "int main(){return 1 > 2;}"
assert 1 "int main(){return 1 < 2;}"
assert 1 "int main(){return 1 <= 2;}"
assert 1 "int main(){return 1 <= 1;}"
assert 0 "int main(){return 1 >= 2;}"
assert 1 "int main(){return 1 >= 1;}"

assert 1 "int main(){1 == 1;}"
assert 0 "int main(){1 != 1;}"
assert 0 "int main(){(1 == 1) < (1 != 1);}"
assert 1 "int main(){1 == 1 < 1 != 1;}"
assert 1 "int main(){2 >= 2;}"

assert 100 "int main() {return ((32 + -980) <= (-  5*4 * (-2) + 9 -997 * (-2) / (-2))) + 99;}"

assert 3 "int main(){1 + 1; return 1 + 2;}"
assert 5 "int main() {int a; a = 5; return a;}"
assert 55 "int main() {int a; int b; int c; int z; int y; int x; int t; int u; int v; int w;a = 1; b = 2; c = 3; z = 4; y = 5; x = 6; t = 10; u = 9; v = 8; w = 7; return a +  b + c + z + y + x + t + u + v + w;}"
assert 3 "int main() {int abc ;abc = 22; int cde;cde=7;return  abc / cde;}"
assert 1 "int main() {int abc; abc = 22; int cde;cde=7; return abc > cde;}"
assert 4 "int main(){return 4; return 5;}"


assert 3 "int main(){ int a; a = 1; if (44 > 32) a = 3; if(44 < 32) a = 5; return a;}"
assert 100 "int main() { int a;a = 5; if (55 != 43) a = 100; else a = 50; return a;}"

assert 1 "int main() {int i;i = 0; int sum;sum = 1; while(i <= 10) i = 11; return sum;}"
assert 64 "int main() {int i;i = 1; while(i <=55) i = i*2; return i;}"

assert 1 "int main() {return 10 <= 10;}"

assert 55 "int main() {int sum;sum = 0; int i;for (i = 1; i <= 10; i = i + 1) sum = sum + i; return sum;}"
assert 1 "int main(){ int sum;sum = 0; int i;for (i = 1; i == 1; i = i + 1) sum = sum + i; return sum;}"

# remains
assert 1 "int main() { return 6 % 3 == 0;}"
assert 3 "int main() { return 6 / 2 % (3 + 1);}"
assert 4 "int main()  {int a;a = 6 / 2 % (3 + 1); return a + 1;}"

assert 0 "int main() {return 103 % 102 == 0;}"
assert 1 "int main() {int n; int i; int result;n = 103; i = 102; result = 1; if (n % i == 0) {result = 0;} return result;}"
assert 102 "int main() {int v; int i;v =0; for (i = 2; i < 103; i = i + 1) {v = i;} return v;}"
assert 1 "int main() {int n; int result;n = 103; int i; result = 1; for (i = 2; i < 103; i = i + 1) {if (n % i == 0) {result = 0;}} return result;}"

assert 55 "int main() {int sum; int i;sum = 0; i = 1; while(i <= 10) {sum = sum + i; i = i + 1;} return sum;}"
assert 102 "int main()   {int sum; int i; sum  = 5;i  = 10;while (i * 2 <= 90000){if ((i + 5) % 4 == 0){i = i + 2;sum = sum + i + 2;}i = i * 2;sum = sum + i + 9;}return sum % 256;}"

assert 10 "int main() {int a; int i; int j;a  = 1; for (i = 0; i < 2; i = i + 1) {for (j = 0; j < 3; j = j + 1) {a = a + i + j;}} return a;}"

assert 105 "int main(){int j; int sum; int i;sum = 4; i = 0; while (i < 74) {for (j = 1; j < 10; j = j + 1) { sum = sum + j;}i = i + 3;} return sum % 256;}"
 
assert 2 "int just2();int main(){return just2();}"
assert 198 "int just_ret(int a);int main(){return just_ret(198);}"

assert 1 "int main(){int a; int b;b = 0; a= 100; a=b=1; return a == 1;}"


assert 4 "int just4() {return 4;} int main() {return just4();}"
assert 10 "int double_it(int i) {return 2*i;} int main() {return double_it(5);}"
assert 27 "int mul(int  a, int  b) {return a*b;} int main() {return mul(3, 9);}"
assert 1 "int add6(int a, int b, int c, int d, int e, int f) {return a+b+c+d+e+f;} int main() {int val; int i;val = 0; for (i = 0; i <= 6; i = i + 1) {val = val + i;}return add6(1, 2, 3, 4, 5, 6) == val;}"
assert 21 "int add6(int a, int b, int c, int d, int e, int f) {return a+b+c+d+e+f;} int main() {int val; int i;val = 0; for (i = 0; i <= 6; i = i + 1) {val = val + i;}return add6(1, 2, 3, 4, 5, 6);}"

assert 21 "int main() {int val; int i;val = 0; for (i = 0; i <= 6; i = i + 1) {val = val + i;}return val;}"

assert 5 "int main() {int b; b = 3; int*pb;pb = &b; b = 5; return *pb;}"

assert 3 "int main() {int x; int y; int *z;x = 3; y = 5; z = &y + 1; return  *z;}"
assert 3 "int main() {int x; int *px; int **ppx;x = 3; px = &x; ppx = &px; return **ppx;}"
assert 3 "int main() {int x; int *px; int **ppx;x = 3; px = &x; ppx = &px; return **ppx;}"
assert 7 "int main() {int x; int *px;x = 3; int y; int z;y = 4; px = &x; int *py;py = &y; z = *px + *py; return z;}"

assert 1 "int*alloc4(int a, int b, int c, int d);int main() {int *p; p = alloc4(1, 2, 3, 4);  return *p;}"

assert 7 "int sub(int a, int b) {return a - b;} int main() {return sub(15, 8);}"

assert 3 "int*alloc4(int a, int b, int c, int d);int main() {int *p; p = alloc4(1, 2, 3, 4);  return *(p + 2);}"

assert 3 "int main() {int x;x=5; int *p;  p = &x; *p = 3; return x;}"
assert 10 "int no(){}int main() {int x;x=5; int y; y = 10; int *px; px= &x; int *py; py = &y; int **ppx; ppx = &px; no(); *ppx = py;  return **ppx;} "

assert 10  "int main() {
    int x;
    x = 5;
    int y;
    y = 10;
    int *px;
    int *py;
    px = &x;
    py = &y;
    int **z;
    z = &px;
    if (**z == 5)
    {
        *z = py;
    }
    return **z;}"

assert 0 "int global_var; int main() {return global_var;}"
assert 6 "int global_var; int main() {global_var = 6;return global_var;}"
assert 9 "int global_var[2]; int main() {global_var[1] = 9;return *(global_var + 1);}"

wait

check
status="$?"

clean
exit $status