section .text
    global _start

_start:
    ; exit(0)
    mov eax, 1         ; system call for sys_exit
    ;xor ebx, ebx       ; exit code 0
    mov ebx, 5         ; exit code 1
    int 0x80           ; call kernele