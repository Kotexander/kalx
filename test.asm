extern printf               ; Declare the printf function from the C library
extern square

section .data
    format db "Hello, %i!", 10, 0    ; Define the format string with null terminator

section .text
    global main

main:
    push ebp
    mov ebp, esp

    call square
    call square
    call square

    push dword 32                     ; Push the address of the name string
    push dword format                 ; Push the address of the format string
    call printf                       ; Call the printf function
    add esp, 8

    cmp dword [ebp-12], 10000

    mov eax, [ebp+eax*4+32];

    mov eax, 5
    pop ebp
    ret
