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
       	movl	$0,	%ecx
       	# call
       	# buckup
       	movl	%ecx,	8(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	allocate
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	8(%esp),	%ecx
       	# restore end
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	movl	$2,	%ebx
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
       	call	Arrmake
       	movl	%eax,	%esi
       	addl	$8,	%esp
       	# restore
       	movl	8(%esp),	%ecx
       	movl	12(%esp),	%ebx
       	# restore end
       	# buckup
       	movl	%ecx,	8(%esp)
       	movl	%ebx,	12(%esp)
       	movl	%esi,	16(%esp)
       	# buckup end


push %esi
push %ebx
push %ecx
push %ecx
call write
pop %eax
pop %ecx
pop %ebx
pop %esi


       	# gc args
       	subl	$4,	%esp
       	movl	%ebx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	addl	$4,	%esp
       	# gc args end

push %esi
push %ebx
push %ecx
push $5
call write
pop %eax
pop %ecx
pop %ebx
pop %esi

       	# restore
       	movl	8(%esp),	%ecx
       	movl	12(%esp),	%ebx
       	movl	16(%esp),	%esi
       	# restore end
       	movl	$0,	%eax
       	movl	%eax,	%ebx
       	movl	$0,	%eax
       	movl	%eax,	%ecx
       	movl	%esi,	%eax
       	movl	%eax,	%ecx
       	# call end
       	movl	%ecx,	%eax
       	movl	0(%esp),	%edx
       	movl	%eax,	0(%esp)
       	movl	%edx,	%ecx
       	movl	%ecx,	%ecx
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
       	movl	%ecx,	%ecx
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
       	movl	0(%esp),	%eax
       	movl	%eax,	%ecx
       	movl	%ecx,	%ebx
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
       	addl	$4,	%esp
       	# gc args end
       	# restore
       	movl	8(%esp),	%ecx
       	movl	12(%esp),	%ebx
       	# restore end
       	movl	$0,	%eax
       	movl	%eax,	%ecx
       	movl	%ebx,	%eax
       	movl	%eax,	%ecx
       	# call end
       	movl	%ecx,	%ecx
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
       	# call
       	call	ensure_no_allocations
       	movl	%eax,	%ecx
       	# buckup
       	movl	%ecx,	8(%esp)
       	# buckup end
       	# gc args
       	subl	$4,	%esp
       	addl	$4,	%esp
       	# gc args end
       	# restore
       	movl	8(%esp),	%ecx
       	# restore end
       	movl	%ecx,	%eax
       	# call end
       	movl	%ecx,	%ecx
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
