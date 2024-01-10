// #include <stdio.h>

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
    int i = 0;
    int* f = &i + 1;
    int k = i + *f;

    int r[] = {0};
    int q = r[0] + r[1]; 

    while (i < 10) {
        i = i + 1;
        int j = 0;
        while (j < 3) {
            int x = 0;
            j = j + 1;
            x += j;
        }
    }
}