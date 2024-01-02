#!/bin/fish

nasm -felf32 test.asm -o output/test.o -g
if test $status -ne 0
    echo "failed to assemble"
    exit 1
end

gcc -m32 -c ctest.c -o output/ctest.o -g
if test $status -ne 0
    echo "failed to compile"
    exit 1
end

gcc -m32 output/test.o output/ctest.o -o output/test -no-pie -g
if test $status -ne 0
    echo "failed to link"
    exit 1
end

echo "running..."
output/test
