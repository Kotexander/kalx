# Running
```
cargo run
ld -m elf_i386 output/main.o -o output/main
output/main
```
Quickly run:  
`cargo run && ld -m elf_i386 output/main.o -o output/main && output/main`

`objdump -M intel -D <file>`

`nasm -felf32 output/hello.asm`