.section     .text
.global      main

main:
 subl	$8,	%esp
 call	read
 movl	%eax,	0(%esp)
 call	read
 movl	%eax,	4(%esp)
 movl	0(%esp),	%eax
 pushl	%eax
 movl	8(%esp),	%eax
 pushl	%eax
 popl	%edi
 popl	%esi
 addl	%esi,	%edi
 pushl	%edi
 call	write
 popl	%eax
 addl	$8,	%esp
 xorl	%eax,	%eax
 ret
