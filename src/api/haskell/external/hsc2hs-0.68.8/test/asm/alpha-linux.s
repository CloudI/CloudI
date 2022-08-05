	.set noreorder
	.set volatile
	.set noat
	.set nomacro
	.arch ev4
	.globl ___hsc2hs_BOM___
	.section	.sdata,"aws",@progbits
	.align 3
	.type	___hsc2hs_BOM___, @object
	.size	___hsc2hs_BOM___, 8
___hsc2hs_BOM___:
	.quad	4294967296
	.globl x___hsc2hs_sign___
	.section	.sbss,"aw"
	.type	x___hsc2hs_sign___, @object
	.size	x___hsc2hs_sign___, 8
	.align 3
x___hsc2hs_sign___:
	.zero	8
	.globl x
	.section	.sdata
	.align 3
	.type	x, @object
	.size	x, 8
x:
	.quad	1
	.globl y___hsc2hs_sign___
	.section	.sbss,"aw"
	.type	y___hsc2hs_sign___, @object
	.size	y___hsc2hs_sign___, 8
	.align 3
y___hsc2hs_sign___:
	.zero	8
	.globl y
	.section	.sdata
	.align 3
	.type	y, @object
	.size	y, 8
y:
	.quad	-1
	.globl z___hsc2hs_sign___
	.align 3
	.type	z___hsc2hs_sign___, @object
	.size	z___hsc2hs_sign___, 8
z___hsc2hs_sign___:
	.quad	1
	.globl z
	.align 3
	.type	z, @object
	.size	z, 8
z:
	.quad	-1
	.globl t
	.section	.rodata
$LC0:
	.string	"Hello World\" 12345"
	.section	.sdata
	.align 3
	.type	t, @object
	.size	t, 8
t:
	.quad	$LC0
	.ident	"GCC: (Gentoo 7.2.0-r1 p1.1) 7.2.0"
	.section	.note.GNU-stack,"",@progbits
