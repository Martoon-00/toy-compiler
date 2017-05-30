	       .section     .text
	       .global      main
	       
	       	int3
	       	main:
	       	subl	$24,	%esp
	       	call	read
	       	movl	%eax,	%ecx
	       	movl	%eax,	4(%esp)
	       	movl	%eax,	%ecx
	       	subl	$4,	%esp
	       	movl	%ecx,	%eax
	       	movl	%eax,	0(%esp)
	       	call	write
	       	addl	$4,	%esp
	       	movl	%eax,	%ecx
	       	movl	%eax,	0(%esp)
	       	movl	$0,	%ecx
	       	movl	%ecx,	%eax
	       	jmp     main_exit
	       	main_exit:
	       	addl	$24,	%esp
	       	ret
