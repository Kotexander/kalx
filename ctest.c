#include <stdio.h>
void square() {
    int a = 5;
    int b = 10;
    int c = 15;
    int z = a - (b - a) / (c - a);
    printf("square!\n");
}