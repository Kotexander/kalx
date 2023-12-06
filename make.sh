nasm -f elf32 exit.asm -o exit.o
ld -m elf_i386 exit.o -o exit