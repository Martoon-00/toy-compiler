       .section     .text
       .global      main

       	int3
       	main:
       	subl	$20,	%esp
       	movl	$0,	%ecx
       	movl	0(%esp),	%ebx
       	# buckup
       	movl	%ecx,	4(%esp)
       	movl	%ebx,	8(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ebx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	%eax,	%esi
       	addl	$4,	%esp
       	# restore
       	movl	4(%esp),	%ecx
       	movl	8(%esp),	%ebx
       	# restore end
       	movl	%esi,	%esi
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	movl	$0,	%ecx
       	# buckup
       	movl	%ecx,	4(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	allocate
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	4(%esp),	%ecx
       	# restore end
       	movl	%ebx,	%ebx
       	movl	0(%esp),	%ebx
       	# buckup
       	movl	%ecx,	4(%esp)
       	movl	%ebx,	8(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ebx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_decrement
       	movl	%eax,	%esi
       	addl	$4,	%esp
       	# restore
       	movl	4(%esp),	%ecx
       	movl	8(%esp),	%ebx
       	# restore end
       	movl	%esi,	%esi
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	movl	%eax,	%ecx
       	movl	%ecx,	%ebx
       	# buckup
       	movl	%ecx,	4(%esp)
       	movl	%ebx,	8(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ebx,	%eax
       	movl	%eax,	0(%esp)
       	call	ref_counter_increment
       	movl	%eax,	%esi
       	addl	$4,	%esp
       	# restore
       	movl	4(%esp),	%ecx
       	movl	8(%esp),	%ebx
       	# restore end
       	movl	%esi,	%esi
       	# buckup
       	movl	%ecx,	4(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	%eax
       	movl	%eax,	0(%esp)
       	call	free
       	movl	%eax,	%ebx
       	addl	$4,	%esp
       	# restore
       	movl	4(%esp),	%ecx
       	# restore end
       	# buckup
       	movl	%ecx,	4(%esp)
       	movl	%ebx,	8(%esp)
       	# buckup end
       	subl	$4,	%esp
       	movl	%ecx,	0(%esp)
       	call	ref_counter_decrement
       	addl	$4,	%esp
       	# restore
       	movl	4(%esp),	%ecx
       	movl	8(%esp),	%ebx
       	# restore end
       	movl	%ebx,	%ebx
       	call	ensure_no_allocations
       	movl	%eax,	%ecx
       	# buckup
       	movl	%ecx,	4(%esp)
       	# buckup end
       	subl	$4,	%esp
       	addl	$4,	%esp
       	# restore
       	movl	4(%esp),	%ecx
       	# restore end
       	movl	%ecx,	%ebx
       	movl	$0,	%ecx
       	movl	%ecx,	%eax
       	jmp     10f
       	10:
       	addl	$20,	%esp
       	ret
