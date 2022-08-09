#include <stdio.h>
#include <stdlib.h>

void print_ok()
{
    printf("\033[32mALL TESTS PASSED\033[0m\n");
}

int printf2(char *msg, int arg0, int arg1)
{
    printf(msg, arg0, arg1);
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

int **alloc4_ptr(int *a, int *b, int *c, int *d)
{
    int **p = malloc(sizeof(int *) * 4);
    p[0] = a;
    p[1] = b;
    p[2] = c;
    p[3] = d;
    return p;
}

void *print_p(void *p)
{
    printf("%p\n", p);
    return p;
}
