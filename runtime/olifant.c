#include "stdio.h"

int olifant();

/* Math FFI */
int sum(int x, int y) {return x + y;}
int sub(int x, int y) {return x - y;}
int mul(int x, int y) {return x * y;}
int div(int x, int y) {return x / y;}
int shr(int x, int y) {return x >> y;}
int shl(int x, int y) {return x << y;}

int main() {
   printf("%d\n", olifant());
   return 0;
}
