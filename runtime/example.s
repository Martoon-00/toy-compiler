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
       	movl	$15,	%ecx
       	movl	%ecx,	%eax
       	movl	12(%esp),	%edx
       	movl	%eax,	12(%esp)
       	movl	%edx,	%ecx
       	movl	%ecx,	%eax
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
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
       	movl	4(%esp),	%edx
       	movl	%eax,	4(%esp)
       	movl	%edx,	%ecx
       	movl	%ecx,	%eax
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
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
       	movl	%eax,	%ebx
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ebx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_increment
       	movl	%eax,	%esi
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	# restore end
       	movl	%esi,	%eax
       	movl	%eax,	%ebx
       	# call end
       	movl	$1,	%ebx
       	movl	12(%esp),	%eax
       	movl	%eax,	%esi
       	movl	%eax,	%edi
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	movl	%esi,	28(%esp)
       	movl	%edi,	32(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%edi,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_increment
       	movl	%eax,	4(%esp)
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	movl	28(%esp),	%esi
       	movl	32(%esp),	%edi
       	# restore end
       	movl	0(%esp),	%eax
       	movl	%eax,	%edi
       	# call end
       	# array set
       	shll	%ebx
       	addl	$1,	%ebx
       	movl	%ecx,	%eax
       	movl	%ebx,	%edx
       	leal	(%eax, %edx, 4),	%eax
       	movl	(%eax),	%edx
       	movl	%edx,	%ebx
       	movl	%esi,	%edx
       	movl	%edx,	(%eax)
       	movl	%ebx,	%eax
       	movl	%eax,	%esi
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	movl	%esi,	28(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%esi,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	%eax,	%edi
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	movl	28(%esp),	%esi
       	# restore end
       	movl	%edi,	%eax
       	movl	%eax,	%esi
       	# call end
       	movl	%ecx,	%eax
       	movl	%eax,	%esi
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	movl	%esi,	28(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%esi,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	%eax,	%edi
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	movl	28(%esp),	%esi
       	# restore end
       	movl	%edi,	%eax
       	movl	%eax,	%esi
       	# call end
       	# array set end
       	movl	4(%esp),	%eax
       	movl	%eax,	%ecx
       	movl	%eax,	%ebx
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ebx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_increment
       	movl	%eax,	%esi
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	# restore end
       	movl	%esi,	%eax
       	movl	%eax,	%ebx
       	# call end
       	movl	%ecx,	%eax
       	movl	12(%esp),	%edx
       	movl	%eax,	12(%esp)
       	movl	%edx,	%ecx
       	movl	%ecx,	%eax
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
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
       	movl	4(%esp),	%edx
       	movl	%eax,	4(%esp)
       	movl	%edx,	%ecx
       	movl	%ecx,	%eax
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
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
       	movl	%eax,	%ebx
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ebx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_increment
       	movl	%eax,	%esi
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	# restore end
       	movl	%esi,	%eax
       	movl	%eax,	%ebx
       	# call end
       	movl	$1,	%ebx
       	movl	12(%esp),	%eax
       	movl	%eax,	%esi
       	movl	%eax,	%edi
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	movl	%esi,	28(%esp)
       	movl	%edi,	32(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%edi,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_increment
       	movl	%eax,	4(%esp)
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	movl	28(%esp),	%esi
       	movl	32(%esp),	%edi
       	# restore end
       	movl	0(%esp),	%eax
       	movl	%eax,	%edi
       	# call end
       	# array set
       	shll	%ebx
       	addl	$1,	%ebx
       	movl	%ecx,	%eax
       	movl	%ebx,	%edx
       	leal	(%eax, %edx, 4),	%eax
       	movl	(%eax),	%edx
       	movl	%edx,	%ebx
       	movl	%esi,	%edx
       	movl	%edx,	(%eax)


push %esi
push %ebx
push %ecx
push %edx
push %eax
push %eax
call debug_write
pop %eax
pop %eax
pop %edx
pop %ecx
pop %ebx
pop %esi


       	movl	%ebx,	%eax
       	movl	%eax,	%esi
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	movl	%esi,	28(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%esi,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	%eax,	%edi
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	movl	28(%esp),	%esi
       	# restore end
       	movl	%edi,	%eax
       	movl	%eax,	%esi
       	# call end
       	movl	%ecx,	%eax
       	movl	%eax,	%esi
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	movl	%esi,	28(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%esi,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	%eax,	%edi
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	movl	28(%esp),	%esi
       	# restore end
       	movl	%edi,	%eax
       	movl	%eax,	%esi
       	# call end
       	# array set end
       	movl	4(%esp),	%eax
       	movl	%eax,	%ecx
       	movl	%eax,	%ebx
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ebx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_increment
       	movl	%eax,	%esi
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	# restore end
       	movl	%esi,	%eax
       	movl	%eax,	%ebx
       	# call end
       	movl	%ecx,	%eax
       	movl	12(%esp),	%edx
       	movl	%eax,	12(%esp)
       	movl	%edx,	%ecx
       	movl	%ecx,	%eax
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	movl	$1,	%ecx
       	movl	%ecx,	%eax
       	movl	16(%esp),	%edx
       	movl	%eax,	16(%esp)
       	movl	%edx,	%ecx
       	movl	%ecx,	%eax
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
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
       	movl	%eax,	%ebx
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ebx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_increment
       	movl	%eax,	%esi
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	# restore end
       	movl	%esi,	%eax
       	movl	%eax,	%ebx
       	# call end


       	movl	$5,	%ebx
       	# <
       	sarl	%ecx
       	sarl	%ebx
       	xor	%edx,	%edx
       	cmp	%ebx,	%ecx
       	setl      %dl
       	movl	%edx,	%ebx
       	shll	%ecx
       	addl	$1,	%ecx
       	shll	%ebx
       	addl	$1,	%ebx
       	# < end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	cmp	$1,	%eax
       	jne     L0
       	jmp     L1
       	L0:
       	L2:

push %esi
push %ebx
push %ecx
push $50
call write
pop %eax
pop %ecx
pop %ebx
pop %esi

       	movl	12(%esp),	%eax
       	movl	%eax,	%ecx
       	movl	%eax,	%ebx
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ebx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_increment
       	movl	%eax,	%esi
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	# restore end
       	movl	%esi,	%eax
       	movl	%eax,	%ebx
       	# call end
       	movl	$1,	%ebx

push %esi
push %ebx
push %ecx
push $60
call write
pop %eax
pop %ecx
pop %ebx
pop %esi

       	# array access
       	shll	%ebx
       	addl	$1,	%ebx
       	movl	%ecx,	%eax
       	movl	%ebx,	%edx
       	movl	(%eax, %edx, 4),	%eax
       	movl	%eax,	%ebx
       	movl	%ecx,	%eax
       	movl	%eax,	%esi
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	movl	%esi,	28(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%esi,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	%eax,	%edi
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	movl	28(%esp),	%esi
       	# restore end
       	movl	%edi,	%eax
       	movl	%eax,	%esi
       	# call end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	movl	%eax,	%esi
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	movl	%esi,	28(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%esi,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_increment
       	movl	%eax,	%edi
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	movl	28(%esp),	%esi
       	# restore end
       	movl	%edi,	%eax
       	movl	%eax,	%esi
       	# call end
       	# array access end
       	movl	%ecx,	%eax
       	movl	12(%esp),	%edx
       	movl	%eax,	12(%esp)
       	movl	%edx,	%ecx
       	movl	%ecx,	%eax
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
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
       	movl	%eax,	%ebx
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ebx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_increment
       	movl	%eax,	%esi
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	# restore end
       	movl	%esi,	%eax
       	movl	%eax,	%ebx
       	# call end
       	movl	$3,	%ebx
       	# +
       	sarl	%ecx
       	sarl	%ebx
       	addl	%ecx,	%ebx
       	shll	%ecx
       	addl	$1,	%ecx
       	shll	%ebx
       	addl	$1,	%ebx
       	# + end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	movl	16(%esp),	%edx
       	movl	%eax,	16(%esp)
       	movl	%edx,	%ecx
       	movl	%ecx,	%eax
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
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
       	movl	%eax,	%ebx
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ebx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_increment
       	movl	%eax,	%esi
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	# restore end
       	movl	%esi,	%eax
       	movl	%eax,	%ebx
       	# call end
       	movl	$5,	%ebx
       	# <
       	sarl	%ecx
       	sarl	%ebx
       	xor	%edx,	%edx
       	cmp	%ebx,	%ecx
       	setl      %dl
       	movl	%edx,	%ebx
       	shll	%ecx
       	addl	$1,	%ecx
       	shll	%ebx
       	addl	$1,	%ebx
       	# < end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	cmp	$1,	%eax
       	jne     L2
       	L1:
       	movl	12(%esp),	%eax
       	movl	%eax,	%ecx
       	movl	%eax,	%ebx
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ebx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_increment
       	movl	%eax,	%esi
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	# restore end
       	movl	%esi,	%eax
       	movl	%eax,	%ebx
       	# call end
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	write
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	# restore end
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	# buckup end
       	# gc args
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	$0,	%eax
       	movl	%eax,	%ecx
       	addl	$4,	%esp
       	# gc args end
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	movl	%ecx,	%eax
       	movl	4(%esp),	%edx
       	movl	%eax,	4(%esp)
       	movl	%edx,	%ecx
       	movl	%ecx,	%eax
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	movl	$0,	%ecx
       	movl	%ecx,	%eax
       	movl	8(%esp),	%edx
       	movl	%eax,	8(%esp)
       	movl	%edx,	%ecx
       	movl	%ecx,	%eax
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	10:
       	movl	4(%esp),	%eax
       	movl	%eax,	%ecx
       	movl	%eax,	%ebx
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ebx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_increment
       	movl	%eax,	%esi
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	# restore end
       	movl	%esi,	%eax
       	movl	%eax,	%ebx
       	# call end
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	free
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	# restore end
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	# buckup end
       	# gc args
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	$0,	%eax
       	movl	%eax,	%ecx
       	addl	$4,	%esp
       	# gc args end
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	movl	%ecx,	%eax
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
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
       	movl	%eax,	%ebx
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ebx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_increment
       	movl	%eax,	%esi
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	# restore end
       	movl	%esi,	%eax
       	movl	%eax,	%ebx
       	# call end
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	free
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	# restore end
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	# buckup end
       	# gc args
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	$0,	%eax
       	movl	%eax,	%ecx
       	addl	$4,	%esp
       	# gc args end
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	movl	%ecx,	%eax
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
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
       	movl	%eax,	%ebx
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ebx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_increment
       	movl	%eax,	%esi
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	# restore end
       	movl	%esi,	%eax
       	movl	%eax,	%ebx
       	# call end
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	free
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	# restore end
       	# buckup
       	movl	%ecx,	20(%esp)
       	movl	%ebx,	24(%esp)
       	# buckup end
       	# gc args
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	$0,	%eax
       	movl	%eax,	%ecx
       	addl	$4,	%esp
       	# gc args end
       	# restore
       	movl	20(%esp),	%ecx
       	movl	24(%esp),	%ebx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	movl	%ecx,	%eax
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	# call
       	call	ensure_no_allocations
       	movl	%eax,	%ecx
       	movl	%eax,	%ecx
       	# call end
       	movl	%ecx,	%eax
       	# call
       	# buckup
       	movl	%ecx,	20(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	20(%esp),	%ecx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	movl	8(%esp),	%eax
       	movl	%eax,	%ecx
       	addl	$36,	%esp
       	ret
       	# Function "main" end
