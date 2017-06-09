#include <stdio.h>

int x = 42;
const int y = 2;
static int c = 3;

int fib(int n) {
    if (n == 0) {
        return 1;
    } else {
        return n * fib(n - 1);
    }
}

int main() {
    printf("hello %d\n", fib(x));
    return 0;
}
