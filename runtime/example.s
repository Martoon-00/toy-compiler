       .section     .text
       .global      main

       	# Function "main"
       	int3
       	main:
       	subl	$24,	%esp
       	movl	$0,	%eax
       	movl	%eax,	0(%esp)
       	movl	$0,	%eax
       	movl	%eax,	4(%esp)
       	movl	$11,	%ecx
       	movl	$7,	%ebx
       	# call
       	# buckup
       	movl	%ecx,	8(%esp)
       	movl	%ebx,	12(%esp)
       	# buckup end
       	subl	$8,	%esp
       	movl	%ebx,	%eax
       	movl	%eax,	0(%esp)
       	movl	%ecx,	%eax
       	movl	%eax,	4(%esp)
       	call	arrmake
       	movl	%eax,	%esi
       	addl	$8,	%esp
       	# restore
       	movl	8(%esp),	%ecx
       	movl	12(%esp),	%ebx
       	# restore end

push %esi
push %ebx
push %ecx
push $30
call debug_write
pop %eax
pop %ecx
pop %ebx
pop %esi

       	# buckup
       	movl	%ecx,	8(%esp)
       	movl	%ebx,	12(%esp)
       	movl	%esi,	16(%esp)
       	# buckup end
       	# gc args
       	subl	$4,	%esp
       	movl	%ebx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	$0,	%eax
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# gc args end
       	# restore
       	movl	8(%esp),	%ecx
       	movl	12(%esp),	%ebx
       	movl	16(%esp),	%esi
       	# restore end
       	# buckup
       	movl	%ecx,	8(%esp)
       	movl	%ebx,	12(%esp)
       	movl	%esi,	16(%esp)
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
       	movl	8(%esp),	%ecx
       	movl	12(%esp),	%ebx
       	movl	16(%esp),	%esi
       	# restore end
       	movl	%esi,	%eax
       	movl	%eax,	%ecx
       	# call end
       	movl	%ecx,	%eax
       	movl	0(%esp),	%edx
       	movl	%eax,	0(%esp)
       	movl	%edx,	%ecx
       	movl	%ecx,	%eax
       	# call
       	# buckup
       	movl	%ecx,	8(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	8(%esp),	%ecx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	movl	$0,	%ecx
       	movl	%ecx,	%eax
       	movl	4(%esp),	%edx
       	movl	%eax,	4(%esp)
       	movl	%edx,	%ecx
       	movl	%ecx,	%eax
       	# call
       	# buckup
       	movl	%ecx,	8(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	8(%esp),	%ecx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	10:


push %esi
push %ebx
push %ecx
push $10
call debug_write
pop %eax
pop %ecx
pop %ebx
pop %esi


        movl	0(%esp),	%eax
       	movl	%eax,	%ecx
       	movl	%eax,	%ebx
       	# call
       	# buckup
       	movl	%ecx,	8(%esp)
       	movl	%ebx,	12(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ebx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_increment
       	movl	%eax,	%esi
       	addl	$4,	%esp
       	# restore
       	movl	8(%esp),	%ecx
       	movl	12(%esp),	%ebx
       	# restore end
       	movl	%esi,	%eax
       	movl	%eax,	%ebx
       	# call end

push %esi
push %ebx
push %ecx
push $5
call debug_write
pop %eax
pop %ecx
pop %ebx
pop %esi

       	# call
       	# buckup
       	movl	%ecx,	8(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	free
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	8(%esp),	%ecx
       	# restore end
       	# buckup
       	movl	%ecx,	8(%esp)
       	movl	%ebx,	12(%esp)
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
       	movl	8(%esp),	%ecx
       	movl	12(%esp),	%ebx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	movl	%ecx,	%eax
       	# call
       	# buckup
       	movl	%ecx,	8(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	8(%esp),	%ecx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end

push %esi
push %ebx
push %ecx
push $1
call debug_write
pop %eax
pop %ecx
pop %ebx
pop %esi

       	# call
       	call	ensure_no_allocations
       	movl	%eax,	%ecx
       	movl	%eax,	%ecx
       	# call end
       	movl	%ecx,	%eax
       	# call
       	# buckup
       	movl	%ecx,	8(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	8(%esp),	%ecx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	movl	4(%esp),	%eax
       	movl	%eax,	%ecx
       	addl	$24,	%esp
       	ret
       	# Function "main" end
