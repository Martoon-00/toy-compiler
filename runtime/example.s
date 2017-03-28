.section     .text
.global      main

main:
    call read
    addl $1, %eax
    subl $4, %esp
    movl $10, (%esp)
    call write
    popl %eax
    xorl %eax, %eax
    ret
