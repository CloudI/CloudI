	.text
	.file	"tmp.c"
	.type	___hsc2hs_BOM___,@object // @___hsc2hs_BOM___
	.data
	.globl	___hsc2hs_BOM___
	.p2align	3
___hsc2hs_BOM___:
	.xword	4294967296              // 0x100000000
	.size	___hsc2hs_BOM___, 8

	.type	x___hsc2hs_sign___,@object // @x___hsc2hs_sign___
	.bss
	.globl	x___hsc2hs_sign___
	.p2align	3
x___hsc2hs_sign___:
	.xword	0                       // 0x0
	.size	x___hsc2hs_sign___, 8

	.type	x,@object               // @x
	.data
	.globl	x
	.p2align	3
x:
	.xword	1                       // 0x1
	.size	x, 8

	.type	y___hsc2hs_sign___,@object // @y___hsc2hs_sign___
	.bss
	.globl	y___hsc2hs_sign___
	.p2align	3
y___hsc2hs_sign___:
	.xword	0                       // 0x0
	.size	y___hsc2hs_sign___, 8

	.type	y,@object               // @y
	.data
	.globl	y
	.p2align	3
y:
	.xword	-1                      // 0xffffffffffffffff
	.size	y, 8

	.type	z___hsc2hs_sign___,@object // @z___hsc2hs_sign___
	.globl	z___hsc2hs_sign___
	.p2align	3
z___hsc2hs_sign___:
	.xword	1                       // 0x1
	.size	z___hsc2hs_sign___, 8

	.type	z,@object               // @z
	.globl	z
	.p2align	3
z:
	.xword	-1                      // 0xffffffffffffffff
	.size	z, 8

	.type	.L.str,@object          // @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Hello World\" 12345"
	.size	.L.str, 19

	.type	t,@object               // @t
	.data
	.globl	t
	.p2align	3
t:
	.xword	.L.str
	.size	t, 8


	.ident	"clang version 5.0.1 (tags/RELEASE_501/final)"
	.section	".note.GNU-stack","",@progbits
