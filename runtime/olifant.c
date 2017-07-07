#include "stdio.h"

extern int printi(int X) {
  printf("%d\n", X);
  fflush(stdout);
  return 0;
}
