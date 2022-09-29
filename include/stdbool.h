#ifndef __STDBOOL_H
#define __STDBOOL_H

#ifdef __STDC__

#include <stdbool.h>

#else

typedef enum {
    false,
    true
} bool;

#endif

#endif