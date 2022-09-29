#ifndef __STDBOOL_H
#define __STDBOOL_H

#ifdef __STDC__

#include <stdbool.h>

#else

typedef int bool;
#define true 1
#define false 0

#endif

#endif