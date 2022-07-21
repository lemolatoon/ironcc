#include <stdio.h>
#include <stdlib.h>

void assertion_failed(int index, int expected, int got)
{
    printf("Assertion_failed At test%d\n", index);
    printf("Expected %d, but got %d\n", expected, got);
    exit(index);
}

void passed(int index) { printf("Test %d passed\n", index); }

void print_ok()
{
    printf("\033[32mALL TESTS PASSED\033[0m\n");
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

int assert(int index, int expected, int got)
{
    if (expected != got)
    {
        assertion_failed(index, expected, got);
    }
    else
    {
        passed(index);
    }
    return 0;
}