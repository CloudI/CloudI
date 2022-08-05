	.file	"tmp.c"
	.text
	.globl	___hsc2hs_BOM___
	.data
	.align 8
___hsc2hs_BOM___:
	.quad	4294967296
	.globl	x___hsc2hs_sign___
	.bss
	.align 8
x___hsc2hs_sign___:
	.space 8
	.globl	x
	.data
	.align 8
x:
	.quad	1
	.globl	y___hsc2hs_sign___
	.bss
	.align 8
y___hsc2hs_sign___:
	.space 8
	.globl	y
	.data
	.align 8
y:
	.quad	-1
	.globl	z___hsc2hs_sign___
	.align 8
z___hsc2hs_sign___:
	.quad	1
	.globl	z
	.align 8
z:
	.quad	-1
	.globl	t
	.section .rdata,"dr"
.LC0:
	.ascii "Hello World\" 12345\0"
	.data
	.align 8
t:
	.quad	.LC0
	.ident	"GCC: (GNU) 7.3.0"
