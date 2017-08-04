#include "stdio.h"
#include "stdbool.h"

int olifant();

/* Math FFI */
int sum(int x, int y) {return x + y;}
int sub(int x, int y) {return x - y;}
int mul(int x, int y) {return x * y;}
int div(int x, int y) {return x / y;}
int shr(int x, int y) {return x >> y;}
int shl(int x, int y) {return x << y;}

bool lt(int x, int y) {return x < y;}
bool gt(int x, int y) {return x > y;}
bool eq(int x, int y) {return x == y;}

int main() {
   printf("%d\n", olifant());
   return 0;
}
