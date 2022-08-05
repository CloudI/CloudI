	.text
	.abicalls
	.section	.mdebug.abi64,"",@progbits
	.nan	legacy
	.file	"tmp.c"
	.type	___hsc2hs_BOM___,@object
	.data
	.globl	___hsc2hs_BOM___
	.p2align	3
___hsc2hs_BOM___:
	.8byte	4294967296
	.size	___hsc2hs_BOM___, 8

	.type	x___hsc2hs_sign___,@object
	.bss
	.globl	x___hsc2hs_sign___
	.p2align	3
x___hsc2hs_sign___:
	.8byte	0
	.size	x___hsc2hs_sign___, 8

	.type	x,@object
	.data
	.globl	x
	.p2align	3
x:
	.8byte	1
	.size	x, 8

	.type	y___hsc2hs_sign___,@object
	.bss
	.globl	y___hsc2hs_sign___
	.p2align	3
y___hsc2hs_sign___:
	.8byte	0
	.size	y___hsc2hs_sign___, 8

	.type	y,@object
	.data
	.globl	y
	.p2align	3
y:
	.8byte	-1
	.size	y, 8

	.type	z___hsc2hs_sign___,@object
	.globl	z___hsc2hs_sign___
	.p2align	3
z___hsc2hs_sign___:
	.8byte	1
	.size	z___hsc2hs_sign___, 8

	.type	z,@object
	.globl	z
	.p2align	3
z:
	.8byte	-1
	.size	z, 8

	.type	.L.str,@object
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Hello World\" 12345"
	.size	.L.str, 19

	.type	t,@object
	.data
	.globl	t
	.p2align	3
t:
	.8byte	.L.str
	.size	t, 8


	.ident	"clang version 5.0.1 (tags/RELEASE_501/final)"
	.section	".note.GNU-stack","",@progbits
	.text
