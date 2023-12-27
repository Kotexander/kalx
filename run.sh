#!/bin/fish

cargo run

if test $status -ne 0
    exit 1
end

gcc -m32 output/main.o -o output/main -no-pie
if test $status -ne 0
    echo "failed to link"
    exit 1
end

echo "running..."
output/main
