	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 4
	.section	__DATA,__data
	.globl	____hsc2hs_BOM___       ## @___hsc2hs_BOM___
	.p2align	3
____hsc2hs_BOM___:
	.quad	4294967296              ## 0x100000000

	.globl	_x___hsc2hs_sign___     ## @x___hsc2hs_sign___
.zerofill __DATA,__common,_x___hsc2hs_sign___,8,3
	.globl	_x                      ## @x
	.p2align	3
_x:
	.quad	1                       ## 0x1

	.globl	_y___hsc2hs_sign___     ## @y___hsc2hs_sign___
.zerofill __DATA,__common,_y___hsc2hs_sign___,8,3
	.globl	_y                      ## @y
	.p2align	3
_y:
	.quad	-1                      ## 0xffffffffffffffff

	.globl	_z___hsc2hs_sign___     ## @z___hsc2hs_sign___
	.p2align	3
_z___hsc2hs_sign___:
	.quad	1                       ## 0x1

	.globl	_z                      ## @z
	.p2align	3
_z:
	.quad	-1                      ## 0xffffffffffffffff

	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	"Hello World\" 12345"

	.section	__DATA,__data
	.globl	_t                      ## @t
	.p2align	3
_t:
	.quad	L_.str


.subsections_via_symbols
