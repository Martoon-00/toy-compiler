	       .section     .text
	       .global      main
	  
      	int3
       	testfunc:
       	subl	$4,	%esp
       	movl	$0,	%ecx
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	movl	$0,	%ecx
       	movl	$0,	%ebx
       		# cmp   
       	xor	%eax,	%eax
       	cmp	%ecx,	%ebx
       	sete      %al   
       	movl	%eax,	%ebx
       	movl	%eax,	%ecx
       	cmp	$0,	%eax
       	jne     L0    
       	jmp     L1    
       	L0:
       	L2:
       	movl	0(%esp),	%eax
       	movl	%eax,	%ecx
       	movl	$12,	%ebx
       		# cmp   
       	xor	%eax,	%eax
       	cmp	%ecx,	%ebx
       	setl      %al   
       	movl	%eax,	%ebx
       	movl	%eax,	%ecx
       	cmp	$0,	%eax
       	jne     L3    
       	jmp     L4    
       	L3:
       	movl	0(%esp),	%eax
       	movl	%eax,	%ecx
       	jmp     testfunc_exit
       	L4:
       	movl	0(%esp),	%eax
       	movl	%eax,	%ecx
       	movl	$5,	%ebx
       	addl	%ecx,	%ebx
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	movl	%eax,	0(%esp)
       	movl	$0,	%ecx
       	movl	$0,	%ebx
       		# cmp   
       	xor	%eax,	%eax
       	cmp	%ecx,	%ebx
       	sete      %al   
       	movl	%eax,	%ebx
       	movl	%eax,	%ecx
       	cmp	$0,	%eax
       	jne     L2    
       	L1:
       	movl	$0,	%ecx
       	movl	%ecx,	%eax
       	jmp     testfunc_exit
       	testfunc_exit:
       	addl	$4,	%esp
       	ret
       	int3
       	main:
       	subl	$0,	%esp
       	call	testfunc
       	addl	$0,	%esp
       	movl	%eax,	%ecx
       	pushl	%ecx
       	call	write
       	addl	$4,	%esp
       	movl	%eax,	%ecx
       	movl	$0,	%ecx
       	movl	%ecx,	%eax
       	jmp     main_exit
       	main_exit:
       	addl	$0,	%esp
       	ret
