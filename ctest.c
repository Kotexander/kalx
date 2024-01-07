#include <stdio.h>

int cube(int x) {
    return x*x*x;
}

void test(int t) {
    int x = 3;
    int y = 7;
    int z = 5;

    int z_xy = z-x * y / t;

    char str[] = "hello";
    // str[t] = '0';
}


void square() {
    int a = 5-2;
    int b = 10;
    int c = 15;
    int z = a - (b - a) / (c - a);
    int* w = &z;
    printf("square! w*w=%u\n", *w);

    test(3);

    for(int i = 0; i < 10; i++) {
        int x = a * cube(c) - cube(b);
        test(x);
    }
    for(int i = 0; i < 10; i++) {
        int x = c * b - a;
        test(x);
    }
}