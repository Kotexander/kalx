#!/bin/fish

nasm -felf32 test.asm -o output/test.o

if test $status -ne 0
    echo "failed to link"
    exit 1
end

gcc -m32 output/test.o -o output/test -no-pie
if test $status -ne 0
    echo "failed to assemble"
    exit 1
end

echo "running..."
output/test
