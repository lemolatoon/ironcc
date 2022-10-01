#ifndef _ASSERT_H

#define _ASSERT_H 1

#include <stdio.h>
#include<stdlib.h>

// void assert(int expression) {
//     if (!expression) {
//         perror("Assertion failed.");
//         abort();
//     }
// }

void assert_t(int expression);
#define assert assert_t


#endif