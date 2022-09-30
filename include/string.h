#ifndef _STRING_H
#define _STRING_H

#include <stddef.h>

int strncmp(const char *s1, const char *s2, int n);
int strcmp(const char *s1, const char *s2);
size_t strlen(const char *s);
#endif