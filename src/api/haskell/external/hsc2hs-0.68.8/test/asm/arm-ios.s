	.section	__TEXT,__text,regular,pure_instructions
	.ios_version_min 5, 0
	.syntax unified
	.section	__DATA,__data
	.globl	____hsc2hs_BOM___       @ @___hsc2hs_BOM___
	.p2align	3
____hsc2hs_BOM___:
	.long	0                       @ 0x100000000
	.long	1

	.globl	_x___hsc2hs_sign___     @ @x___hsc2hs_sign___
.zerofill __DATA,__common,_x___hsc2hs_sign___,8,3
	.globl	_x                      @ @x
	.p2align	3
_x:
	.long	1                       @ 0x1
	.long	0

	.globl	_y___hsc2hs_sign___     @ @y___hsc2hs_sign___
.zerofill __DATA,__common,_y___hsc2hs_sign___,8,3
	.globl	_y                      @ @y
	.p2align	3
_y:
	.long	4294967295              @ 0xffffffff
	.long	0

	.globl	_z___hsc2hs_sign___     @ @z___hsc2hs_sign___
	.p2align	3
_z___hsc2hs_sign___:
	.long	1                       @ 0x1
	.long	0

	.globl	_z                      @ @z
	.p2align	3
_z:
	.long	4294967295              @ 0xffffffffffffffff
	.long	4294967295

	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 @ @.str
	.asciz	"Hello World\" 12345"

	.section	__DATA,__data
	.globl	_t                      @ @t
	.p2align	2
_t:
	.long	L_.str


.subsections_via_symbols
