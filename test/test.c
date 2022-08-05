int test0();
int test0_2();
int test0_3();
int test1();
int test2(int arg0);
int test3(int arg0);
int test4(int arg0);
int test5(int arg0);
int test6(int arg0);
int test7(int arg0);
int test8();
int test9();
int test9_0();
int test9_1();
int test10();
int test11();
int test12();
int test13();
int test14();
int test15();
int test16();
int test17();
int test18();
int test19();
int test20();
int test21();
int test22();
// actually `void`
int assert(int index, int expected, int got);
// this is comment for the test of function of comment

int f(int arg0);
int g(int arg0);
int h(int arg0);

int sub(int arg0, int arg1);
int add(int arg0, int arg1);

int **alloc4_ptr(int *a, int *b, int *c, int *d);
int *alloc4(int a, int b, int c, int d);

int print_ok();

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
    assert(15, 0, test15());
    assert(16, 0, test16());
    assert(17, 0, test17());
    assert(18, 0, test18());
    assert(19, 0, test19());
    assert(20, 0, test20());
    assert(21, 0, test21());
    assert(22, 0, test21());

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

int test15()
{
    int *first_ptr = alloc4(1, 2, 3, 4);
    int *last_ptr = first_ptr + 3;
    assert(15, 4, *last_ptr);
    assert(15, 3, last_ptr - first_ptr);
    return 0;
}

int test16()
{
    int array[5];
    *array = 1;
    assert(16, 1, *array);
    assert(16, 1, array[0]);
    return 0;
}

int test17()
{
    int a[2];
    a[0] = 3;
    a[1] = 5;
    assert(16, 3, *a);
    assert(16, 3, a[0]);
    assert(16, 3, 0 [a]);
    assert(16, 5, 1 [a]);
    assert(16, 5, *(a + 1));
    return 0;
}

int init_2d_mat(int (*mat)[2], int a, int b, int c, int d)
{
    mat[0][0] = a;
    mat[0][1] = b;
    mat[1][0] = c;
    mat[1][1] = d;
    return 0;
}

int (*mat_mul_2d(int (*new_mat)[2], int (*lhs)[2], int (*rhs)[2]))[2]
{
    for (int i = 0; i < 2; i = i + 1)
    {
        for (int j = 0; j < 2; j = j + 1)
        {
            new_mat[i][j] = 0;
            for (int k = 0; k < 2; k = k + 1)
            {
                new_mat[i][j] = new_mat[i][j] + lhs[i][k] * rhs[k][j];
            }
        }
    }
    return new_mat;
}

int test18()
{
    assert(18, 0, 0);
    int mat1[2][2];
    init_2d_mat(mat1, 1, 2, 1, 3);
    assert(18, 2, mat1[0][1]);
    int mat2[2][2];
    init_2d_mat(mat2, 3, -2, -1, 1);
    assert(18, 1, mat2[1][1]);
    int multiplied[2][2];
    mat_mul_2d(multiplied, mat1, mat2);
    assert(18, 1, multiplied[0][0]);
    assert(18, 0, multiplied[0][1]);
    assert(18, 0, multiplied[1][0]);
    assert(18, 1, multiplied[1][1]);

    return 0;
}

int test19()
{
    int dummy;
    int array1[3];
    assert(19, 4 * 3, sizeof(array1));
    int *array2[3];
    assert(19, 8 * 3, sizeof(array2));
    int(*array3)[3];
    assert(19, 8, sizeof(array3));
    int **array4[3][5][7];
    assert(19, 8 * 3 * 5 * 7, sizeof(array4));
    return 0;
}

int test20_g_var;
int test20_index;

int inc_test20_index();

int test20()
{
    test20_g_var = 9;
    assert(20, 9, test20_g_var);
    test20_index = 0;
    inc_test20_index();
    assert(20, 20, test20_index);
    return 0;
}

int inc_test20_index()
{
    for (int i = 0; i < 20; i = i + 1)
    {
        test20_index = test20_index + 1;
    }
    return 0;
}

int test21_global_var = 21;
int test21()
{
    assert(21, 21, test21_global_var);
    return 0;
}

int test22_global_with_init[3] = {1, 2, 3};
int test22()
{
    assert(22, 1, test22_global_with_init[0]);
    assert(22, 2, test22_global_with_init[2]);
    assert(22, 3, test22_global_with_init[3]);
    test22_global_with_init[0] = 9;
    assert(22, 9, *test22_global_with_init);
}

int add(int x, int y)
{
    return x + y;
}

int sub(int x, int y)
{
    return x - y;
}
