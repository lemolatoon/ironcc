#ifndef _STDARG_H
#define _STDARG_H 1

#ifdef __STDC__
#include<stdarg.h>
#else
typedef struct __builtin_va_list {
    int gp_offset;
    int fp_offset;
    void *overflow_arg_area;
    void *reg_save_area;
} va_list[1];


void va_start(va_list ap, char *fmt);
#endif
#endif