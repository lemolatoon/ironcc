#ifndef _STDLIB_H

#define _STDLIB_H 1

#define NULL __nullptr

#include <stddef.h>

void *malloc(int size);
void *calloc(size_t n, size_t size);
void exit(int status);
void abort(void);

int strtol(const char *s, char **endptr, int base);

#endif
