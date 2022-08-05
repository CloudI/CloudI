	.file	"tmp.c"
	.machinemode zarch
	.machine "z900"
.globl ___hsc2hs_BOM___
.data
	.align	8
	.type	___hsc2hs_BOM___, @object
	.size	___hsc2hs_BOM___, 8
___hsc2hs_BOM___:
	.quad	4294967296
.globl x___hsc2hs_sign___
.bss
	.align	8
	.type	x___hsc2hs_sign___, @object
	.size	x___hsc2hs_sign___, 8
x___hsc2hs_sign___:
	.zero	8
.globl x
.data
	.align	8
	.type	x, @object
	.size	x, 8
x:
	.quad	1
.globl y___hsc2hs_sign___
.bss
	.align	8
	.type	y___hsc2hs_sign___, @object
	.size	y___hsc2hs_sign___, 8
y___hsc2hs_sign___:
	.zero	8
.globl y
.data
	.align	8
	.type	y, @object
	.size	y, 8
y:
	.quad	-1
.globl z___hsc2hs_sign___
	.align	8
	.type	z___hsc2hs_sign___, @object
	.size	z___hsc2hs_sign___, 8
z___hsc2hs_sign___:
	.quad	1
.globl z
	.align	8
	.type	z, @object
	.size	z, 8
z:
	.quad	-1
.globl t
	.section	.rodata
	.align	2
.LC0:
	.string	"Hello World\" 12345"
	.section	.data.rel.local,"aw",@progbits
	.align	8
	.type	t, @object
	.size	t, 8
t:
	.quad	.LC0
	.ident	"GCC: (Gentoo 7.2.0-r1 p1.1) 7.2.0"
	.section	.note.GNU-stack,"",@progbits
