section .data
    format db "Hello, %s!", 0    ; Define the format string with null terminator
    name db "Assembly", 0        ; Define the name string with null terminator

section .text
    global main
    ; global _start               ; Entry point for the program
    extern printf               ; Declare the printf function from the C library

main:
; _start:
    push dword name             ; Push the address of the name string
    push dword format           ; Push the address of the format string
    call printf                 ; Call the printf function

    ; Exit the program
    mov eax, 1                  ; System call number for sys_exit
    xor ebx, ebx                ; Exit code 0
    int 0x80                    ; Interrupt to exit the program
