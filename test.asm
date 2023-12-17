section .text
   global _start

; section .data
;     msg db 'Hello, World!', 0

_start:
   ; write(1, msg, 13)
   ; mov eax, 4         ; system call for sys_write
   ; mov ebx, 1         ; file descriptor 1 is stdout
   ; mov ecx, msg       ; message to write
   ; mov edx, 13        ; message length
   ; int 0x80           ; call kernel

   ; prologue
   push ebp       ; save old base pointer
	mov ebp, esp   ; set new base pointer

   ; init local vars
	sub esp, 8            ; alloc 4` bytes
   mov DWORD [ebp-4], 10 ; set 0
   
   ; exit 
   mov eax, 1        ; system call for sys_exit
   mov ebx, [ebp-4]  ; exit code
   int 0x80          ; call kernel

   ; epilogue
   mov esp, ebp      ; Clean up local variables
   pop ebp           ; Restore the old base pointer


   mov DWORD [256], 32 