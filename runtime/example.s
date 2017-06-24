       .section     .text
       .global      main

       	# Function "main"
       	int3
       	main:

        mov $5, %eax
        mov $-3, %ecx

        shr %eax
        shr %ecx
        add %eax, %ecx
        shl %ecx
        add $1, %ecx

        push %ecx
        call write
        pop %eax

        xor %eax, %eax
