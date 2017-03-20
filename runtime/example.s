.section     .text
.global      main

main:
    call read
    addl $1, %eax
    pushl %eax
    call write
    popl %eax
    xorl %eax, %eax
    ret
