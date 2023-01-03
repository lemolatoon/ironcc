#include <stdio.h>

void clear() { printf("\e[2J"); }

void reset_cursor() {
  printf("\e[0;0H");
  printf("\e[0;0f");
}