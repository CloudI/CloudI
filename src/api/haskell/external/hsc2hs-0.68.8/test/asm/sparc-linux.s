	.text
	.file	"tmp.c"
	.type	___hsc2hs_BOM___,@object
	.data
	.globl	___hsc2hs_BOM___
	.p2align	3
___hsc2hs_BOM___:
	.word	1
	.word	0
	.size	___hsc2hs_BOM___, 8

	.type	x___hsc2hs_sign___,@object
	.section	.bss,#alloc,#write
	.globl	x___hsc2hs_sign___
	.p2align	3
x___hsc2hs_sign___:
	.word	0
	.word	0
	.size	x___hsc2hs_sign___, 8

	.type	x,@object
	.data
	.globl	x
	.p2align	3
x:
	.word	0
	.word	1
	.size	x, 8

	.type	y___hsc2hs_sign___,@object
	.section	.bss,#alloc,#write
	.globl	y___hsc2hs_sign___
	.p2align	3
y___hsc2hs_sign___:
	.word	0
	.word	0
	.size	y___hsc2hs_sign___, 8

	.type	y,@object
	.data
	.globl	y
	.p2align	3
y:
	.word	0
	.word	4294967295
	.size	y, 8

	.type	z___hsc2hs_sign___,@object
	.globl	z___hsc2hs_sign___
	.p2align	3
z___hsc2hs_sign___:
	.word	0
	.word	1
	.size	z___hsc2hs_sign___, 8

	.type	z,@object
	.globl	z
	.p2align	3
z:
	.word	4294967295
	.word	4294967295
	.size	z, 8

	.type	.L.str,@object
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Hello World\" 12345"
	.size	.L.str, 19

	.type	t,@object
	.data
	.globl	t
	.p2align	2
t:
	.word	.L.str
	.size	t, 4


	.ident	"clang version 5.0.1 (tags/RELEASE_501/final)"
	.section	".note.GNU-stack"
