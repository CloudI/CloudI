	.file	"tmp.c"
	.machinemode esa
	.machine "z900"
.globl ___hsc2hs_BOM___
.data
	.align	8
	.type	___hsc2hs_BOM___, @object
	.size	___hsc2hs_BOM___, 8
___hsc2hs_BOM___:
	.long	1
	.long	0
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
	.long	0
	.long	1
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
	.long	-1
	.long	-1
.globl z___hsc2hs_sign___
	.align	8
	.type	z___hsc2hs_sign___, @object
	.size	z___hsc2hs_sign___, 8
z___hsc2hs_sign___:
	.long	0
	.long	1
.globl z
	.align	8
	.type	z, @object
	.size	z, 8
z:
	.long	-1
	.long	-1
.globl t
	.section	.rodata
	.align	2
.LC0:
	.string	"Hello World\" 12345"
	.section	.data.rel.local,"aw",@progbits
	.align	4
	.type	t, @object
	.size	t, 4
t:
	.long	.LC0
	.ident	"GCC: (Gentoo 7.2.0-r1 p1.1) 7.2.0"
	.section	.note.GNU-stack,"",@progbits
