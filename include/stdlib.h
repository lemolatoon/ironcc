#ifndef _STDLIB_H

#define _STDLIB_H 1

void *NULL;

#include <stddef.h>

void *malloc(int size);
void *calloc(size_t n, size_t size);
void exit(int status);
void abort(void);


#endif
