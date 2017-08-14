       .section     .data
       out_indicator:    .int 1
       
       .section     .text
       .global      main
       
       	# Function "main"
       	int3
       	main:
       	subl	$28,	%esp
       	movl	$-1,	%ecx
       	movl	%ecx,	%eax
       	movl	4(%esp),	%edx
       	movl	%eax,	4(%esp)
       	movl	%edx,	%ecx
  movl	%ecx,	%eax


       	# call  
       	# buckup
       	movl	%ecx,	12(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	12(%esp),	%ecx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	movl	$0,	%eax
       	movl	%eax,	8(%esp)
       	movl	$0,	%ecx
       	movl	%ecx,	%eax
       	movl	8(%esp),	%edx
       	movl	%eax,	8(%esp)
       	movl	%edx,	%ecx
  movl	%ecx,	%eax


  	pushl $0
	  call write
	  popl %eax

       	# call  
       	# buckup
       	movl	%ecx,	12(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	12(%esp),	%ecx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	jmp     10f   
       	20:
       	movl	$3,	out_indicator
       	movl	4(%esp),	%eax
       	movl	%eax,	%ecx
       	movl	%eax,	%ebx
       	# call  
       	# buckup
       	movl	%ecx,	12(%esp)
       	movl	%ebx,	16(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ebx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_increment
       	movl	%eax,	%esi
       	addl	$4,	%esp
       	# restore
       	movl	12(%esp),	%ecx
       	movl	16(%esp),	%ebx
       	# restore end
       	movl	%esi,	%eax
       	movl	%eax,	%ebx
       	# call end
       	movl	%ecx,	%eax
       	movl	8(%esp),	%edx
       	movl	%eax,	8(%esp)
       	movl	%edx,	%ecx
       	movl	%ecx,	%eax
       	# call  
       	# buckup
       	movl	%ecx,	12(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	12(%esp),	%ecx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	movl	$1,	out_indicator
       	jmp     10f   
       	10:
       	# call  
       	call	ensure_no_allocations
       	movl	%eax,	%ecx
       	movl	%eax,	%ecx
       	# call end
       	movl	8(%esp),	%eax
       	movl	%eax,	%ecx
       	addl	$28,	%esp
       	ret
       	# Function "main" end
