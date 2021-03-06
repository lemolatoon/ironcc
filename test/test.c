
int main()
{
    assert(0, 3, test0());
    assert(0, 2, test0_2());
    assert(0, 10, test0_3());
    assert(1, 9, test1());
    assert(2, 5, test2(5));
    assert(3, 5, test3(5));
    assert(4, 2, test4(3));
    assert(4, 3, test4(4));
    assert(5, 10, test5(5));
    assert(6, 2, test6(2));
    assert(7, 0, test7(1));
    assert(7, 0, test7(91));
    assert(7, 1, test7(109));
    assert(8, 3, test8());
    test9_0();
    test9_1();
    assert(9, 10, test9());
    assert(10, 3, test10());
    assert(11, 0, test11());
    assert(12, 0, test12());
    assert(13, 0, test13());
    assert(14, 0, test14());

    print_ok();
    return 0;
}

int test0() { return 3; }

int test0_2() { return g(2); }

int test0_3() { return h(5); }

int g(int n) { return n; }

int h(int n)
{
    if (n > 5)
    {
        return 5;
    }
    else
    {
        return 10;
    }
}

int test1() { return f(5); }

int f(int n)
{
    int i;
    i = 3;
    if (n > 4)
    {
        return f(n - 1) + 2;
    }

    return i + n;
}

int test2(int i)
{
    if (i == 0)
    {
        return 0;
    }
    if (i == 1)
    {
        return 1;
    }

    return i;
}

int test3(int i)
{
    if (i == 0)
    {
        return 0;
    }
    if (i == 1)
    {
        return 1;
    }

    return test3(i - 1) + 1;
}

int test4(int i)
{
    if (i == 0)
    {
        return 0;
    }
    if (i == 1)
    {
        return 1;
    }

    return test4(i - 1) + test4(i - 2);
}

int test5(int i) { return add(i, 1000000) + sub(i, 1000000); }

int test6(int i)
{
    if (i == 0)
    {
        return 1;
    }
    return i * test6(i - 1);
}

int test7(int n)
{
    if (n == 1)
    {
        return 0;
    }

    int result;
    result = 1;
    int i;
    for (i = 2; i < n; i = i + 1)
    {
        if (n % i == 0)
        {
            result = 0;
        }
    }
    return result;
}

int test8()
{
    int x;
    x = 3;
    int *y;
    y = &x;
    return *y;
}

int test9_0()
{
    int y;
    y = 10;
    int *py;
    py = &y;
    int x;
    int *px;
    px = &x;
    int **z;
    z = &px;
    *z = py;
    assert(90, 10, **z);
}

int test9_1()
{
    int y;
    y = 10;
    int *py;
    py = &y;
    int x;
    x = 5;
    int *px;
    px = &x;
    int **z;
    z = &px;
    assert(91, 5, **z);
    *z = py;
    assert(91, 10, **z);
}

int test9()
{
    int x;
    x = 5;
    assert(9, 5, x);
    int y;
    y = 10;
    assert(9, 10, y);
    int *px;
    int *py;
    px = &x;
    py = &y;
    assert(9, 5, *px);
    assert(9, 10, *py);
    int **z;
    z = &px;
    assert(9, 5, **z);
    if (**z == 5)
    {
        *z = py;
        assert(9, 10, **z);
    }
    return **z;
}

int test10()
{
    int x;
    int *y;
    y = &x;
    *y = 3;
    return x;
}

int test11()
{
    int *p;
    p = alloc4(2, 3, 5, 8);
    int *q;
    q = p + 2;
    assert(11, 5, *q);
    q = p + 3;
    assert(11, 8, *q);

    int *p2;
    p2 = alloc4(1, 2, 3, 4);
    assert(11, 4, *(3 + p2));
    return 0;
}

int test12()
{
    int a;
    int b;
    int c;
    int d;
    a = 1;
    b = 2;
    c = 3;
    d = 4;
    int **p;
    p = alloc4_ptr(&a, &b, &c, &d);
    assert(12, 1, **p);
    p = p + 2;
    assert(12, 3, **p);
    p = p - 2;
    int *d2;
    d2 = *(p + 3);
    assert(12, 4, *d2);
    assert(12, 4, **(p + 3));
    **(p + 3) = 9;
    assert(12, 9, **(p + 3));
    int *q;
    int e;
    e = 1;
    q = &e;
    *q = **p + **(p + 1) + **(p + 2) - **(p + 3);
    assert(12, -3, *q);
    return 0;
}

int test13()
{
    assert(13, 4, sizeof(int));
    assert(13, 8, sizeof(int *));
    assert(13, 8, sizeof(int ***));
    int a;
    assert(13, 4, sizeof(a));
    assert(13, 4, sizeof a);
    assert(13, 8, sizeof &a);
    return 0;
}

int test14()
{
    int a = 5;
    assert(14, 5, a);
    return 0;
}

int add(int x, int y) { return x + y; }

int sub(int x, int y) { return x - y; }
