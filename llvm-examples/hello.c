#include <stdio.h>

int x = 12;
const int y = 2;
static int c = 3;

int fact(int n) {
    if (n == 0) {
        return 1;
    } else {
        return n * fact(n - 1);
    }
}

int main() {
    printf("hello %d\n", fact(x));
    return 0;
}
