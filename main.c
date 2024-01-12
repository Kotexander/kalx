#include<stdio.h>

void fibonacci(int);
void test();

int main(int argc, char* argv[]) {
    // fibonacci(48);
    // test();
        printf("arg: %x\n", argv);
        printf("arg: %x\n", argv[0]);
    for (int i = 0; i < argc; i++) {
        printf("arg #%u: %.*s\n", i, 20, *argv);
    }
    // write(0, *(argv+1), 50);
}