	.file	"tmp.c"
	.pred.safe_across_calls p1-p5,p16-p63
	.text
	.global ___hsc2hs_BOM___#
	.section	.sdata,"aws",@progbits
	.align 8
	.type	___hsc2hs_BOM___#, @object
	.size	___hsc2hs_BOM___#, 8
___hsc2hs_BOM___:
	data8	4294967296
	.global x___hsc2hs_sign___#
	.section	.sbss,"aws",@nobits
	.align 8
	.type	x___hsc2hs_sign___#, @object
	.size	x___hsc2hs_sign___#, 8
x___hsc2hs_sign___:
	.skip	8
	.global x#
	.section	.sdata
	.align 8
	.type	x#, @object
	.size	x#, 8
x:
	data8	1
	.global y___hsc2hs_sign___#
	.section	.sbss
	.align 8
	.type	y___hsc2hs_sign___#, @object
	.size	y___hsc2hs_sign___#, 8
y___hsc2hs_sign___:
	.skip	8
	.global y#
	.section	.sdata
	.align 8
	.type	y#, @object
	.size	y#, 8
y:
	data8	-1
	.global z___hsc2hs_sign___#
	.align 8
	.type	z___hsc2hs_sign___#, @object
	.size	z___hsc2hs_sign___#, 8
z___hsc2hs_sign___:
	data8	1
	.global z#
	.align 8
	.type	z#, @object
	.size	z#, 8
z:
	data8	-1
	.global t#
	.section	.rodata
	.align 8
.LC0:
	stringz	"Hello World\" 12345"
	.section	.sdata
	.align 8
	.type	t#, @object
	.size	t#, 8
t:
	data8	.LC0
	.ident	"GCC: (Gentoo 7.3.0 p1.0) 7.3.0"
	.section	.note.GNU-stack,"",@progbits
