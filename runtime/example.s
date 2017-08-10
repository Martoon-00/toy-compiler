.section     .data
       out_indicator:    .int 1
       
       .section     .text
       .global      main
       
       	# Function "main"
       	int3
       	main:
       	subl	$36,	%esp
       	movl	$0,	%eax
       	movl	%eax,	4(%esp)
       	movl	$0,	%eax
       	movl	%eax,	8(%esp)
       	movl	$0,	%eax
       	movl	%eax,	12(%esp)
       	movl	$0,	%eax
       	movl	%eax,	16(%esp)
       	movl	$1,	%ecx
       	# call  
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	allocate
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	movl	%ecx,	%eax
       	movl	12(%esp),	%edx
       	movl	%eax,	12(%esp)
       	movl	%edx,	%ecx
       	movl	12(%esp),	%eax
       	movl	%eax,	%ecx
       	movl	$1,	%ebx
       	movl	$3,	%esi
       	# array set
       	# from31
       	sarl	%ebx
       	# from31 end
       	movl	%ecx,	%eax
       	movl	%ebx,	%edx
       	leal	(%eax, %edx, 4),	%eax
       	movl	(%eax),	%edx
       	movl	%edx,	%ebx
       	movl	%esi,	%edx
       	movl	%edx,	(%eax)
       	# array set end
       	movl	$1,	%ecx
       	# call  
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	allocate
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	movl	%ecx,	%eax
       	movl	16(%esp),	%edx
       	movl	%eax,	16(%esp)
       	movl	%edx,	%ecx
       	movl	16(%esp),	%eax
       	movl	%eax,	%ecx
       	movl	$1,	%ebx
       	movl	$5,	%esi
       	# array set
       	# from31
       	sarl	%ebx
       	# from31 end
       	movl	%ecx,	%eax
       	movl	%ebx,	%edx
       	leal	(%eax, %edx, 4),	%eax
       	movl	(%eax),	%edx
       	movl	%edx,	%ebx
       	movl	%esi,	%edx
       	movl	%edx,	(%eax)
       	# array set end
       	movl	$1,	%ecx
       	# call  
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	allocate
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	movl	%ecx,	%eax
       	movl	8(%esp),	%edx
       	movl	%eax,	8(%esp)
       	movl	%edx,	%ecx
       	movl	8(%esp),	%eax
       	movl	%eax,	%ecx
       	movl	$1,	%ebx
       	movl	12(%esp),	%eax
       	movl	%eax,	%esi
       	# array set
       	# from31
       	sarl	%ebx
       	# from31 end
       	movl	%ecx,	%eax
       	movl	%ebx,	%edx
       	leal	(%eax, %edx, 4),	%eax
       	movl	(%eax),	%edx
       	movl	%edx,	%ebx
       	movl	%esi,	%edx
       	movl	%edx,	(%eax)
       	# array set end
       	movl	8(%esp),	%eax
       	movl	%eax,	%ecx
       	movl	$1,	%ebx
       	movl	16(%esp),	%eax
       	movl	%eax,	%esi
       	# array set
       	# from31
       	sarl	%ebx
       	# from31 end
       	movl	%ecx,	%eax
       	movl	%ebx,	%edx
       	leal	(%eax, %edx, 4),	%eax
       	movl	(%eax),	%edx
       	movl	%edx,	%ebx
       	movl	%esi,	%edx
       	movl	%edx,	(%eax)
       	# array set end
       	movl	$0,	%ecx
       	movl	%ecx,	%eax
       	movl	4(%esp),	%edx
       	movl	%eax,	4(%esp)
       	movl	%edx,	%ecx
       	jmp     10f   
       	20:
       	10:
       	movl	8(%esp),	%eax
       	movl	%eax,	%ecx
       	# call  
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	array_free
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	movl	12(%esp),	%eax
       	movl	%eax,	%ecx
       	# call  
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	array_free
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	movl	16(%esp),	%eax
       	movl	%eax,	%ecx
       	# call  
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	array_free
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	movl	4(%esp),	%eax
       	movl	%eax,	%ecx
       	addl	$36,	%esp
       	ret
       	# Function "main" end
