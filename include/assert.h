#include <stdio.h>
#ifndef _ASSERT_H

#define _ASSERT_H 1

#include<stdlib.h>

void assert(int expression) {
    if (!expression) {
        perror("Assertion failed.");
        abort();
    }
}
#endif