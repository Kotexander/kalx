BD = output

all: $(BD)/main $(BD)/test $(BD)/mainc

run: $(BD)/main
	$^

asm: $(BD)/test
	$^

c: $(BD)/mainc
	$^ test

$(BD)/mainc: $(BD)/mainc.o $(BD)/main.o
	gcc -m32 $^ -o $@ -no-pie

$(BD)/main: $(BD)/main.o
	gcc -m32 $^ -o $@ -no-pie

$(BD)/test: $(BD)/test.o $(BD)/ctest.o
	gcc -m32 $^ -o $@ -no-pie

$(BD)/test.o: test.asm
	nasm -felf32 $^ -o $@ 

$(BD)/mainc.o: main.c
	gcc -m32 -c $^ -o $@ -no-pie

$(BD)/ctest.o: ctest.c
	gcc -m32 -c $^ -o $@ -no-pie

$(BD)/main.o: main.kx src/*
	cargo run

clean:
	rm -r $(BD)/*
