	.LEVEL 1.1
.globl ___hsc2hs_BOM___
	.data
	.align 8
	.type	___hsc2hs_BOM___, @object
	.size	___hsc2hs_BOM___, 8
___hsc2hs_BOM___:
	.word	1
	.word	0
.globl x___hsc2hs_sign___
	.section	.bss
	.align 8
	.type	x___hsc2hs_sign___, @object
	.size	x___hsc2hs_sign___, 8
	.align 8
x___hsc2hs_sign___:
	.block 8
.globl x
	.data
	.align 8
	.type	x, @object
	.size	x, 8
x:
	.word	0
	.word	1
.globl y___hsc2hs_sign___
	.section	.bss
	.align 8
	.type	y___hsc2hs_sign___, @object
	.size	y___hsc2hs_sign___, 8
	.align 8
y___hsc2hs_sign___:
	.block 8
.globl y
	.data
	.align 8
	.type	y, @object
	.size	y, 8
y:
	.word	-1
	.word	-1
.globl z___hsc2hs_sign___
	.align 8
	.type	z___hsc2hs_sign___, @object
	.size	z___hsc2hs_sign___, 8
z___hsc2hs_sign___:
	.word	0
	.word	1
.globl z
	.align 8
	.type	z, @object
	.size	z, 8
z:
	.word	-1
	.word	-1
.globl t
	.section	.rodata
	.align 4
.LC0:
	.stringz	"Hello World\" 12345"
	.section	.data.rel.local,"aw",@progbits
	.align 4
	.type	t, @object
	.size	t, 4
t:
	.word	.LC0
	.ident	"GCC: (Gentoo 7.2.0-r1 p1.1) 7.2.0"
