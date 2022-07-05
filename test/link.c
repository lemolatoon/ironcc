#include <stdio.h>
#include <stdlib.h>

int just2()
{
    return 2;
}

int just_ret(int n)
{
    return n;
}

int *alloc4(int a, int b, int c, int d)
{
    int *p = malloc(sizeof(int) * 4);
    p[0] = a;
    p[1] = b;
    p[2] = c;
    p[3] = d;
    return p;
}