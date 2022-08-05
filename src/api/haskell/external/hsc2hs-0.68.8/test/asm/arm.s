	.text
	.syntax unified
	.eabi_attribute	67, "2.09"	@ Tag_conformance
	.cpu	arm1176jzf-s
	.eabi_attribute	6, 6	@ Tag_CPU_arch
	.eabi_attribute	8, 1	@ Tag_ARM_ISA_use
	.eabi_attribute	9, 1	@ Tag_THUMB_ISA_use
	.fpu	vfpv2
	.eabi_attribute	34, 0	@ Tag_CPU_unaligned_access
	.eabi_attribute	68, 1	@ Tag_Virtualization_use
	.eabi_attribute	17, 1	@ Tag_ABI_PCS_GOT_use
	.eabi_attribute	20, 2	@ Tag_ABI_FP_denormal
	.eabi_attribute	21, 0	@ Tag_ABI_FP_exceptions
	.eabi_attribute	23, 3	@ Tag_ABI_FP_number_model
	.eabi_attribute	24, 1	@ Tag_ABI_align_needed
	.eabi_attribute	25, 1	@ Tag_ABI_align_preserved
	.eabi_attribute	28, 1	@ Tag_ABI_VFP_args
	.eabi_attribute	38, 1	@ Tag_ABI_FP_16bit_format
	.eabi_attribute	18, 4	@ Tag_ABI_PCS_wchar_t
	.eabi_attribute	26, 2	@ Tag_ABI_enum_size
	.eabi_attribute	14, 0	@ Tag_ABI_PCS_R9_use
	.file	"tmp.c"
	.type	___hsc2hs_BOM___,%object @ @___hsc2hs_BOM___
	.data
	.globl	___hsc2hs_BOM___
	.p2align	3
___hsc2hs_BOM___:
	.long	0                       @ 0x100000000
	.long	1
	.size	___hsc2hs_BOM___, 8

	.type	x___hsc2hs_sign___,%object @ @x___hsc2hs_sign___
	.bss
	.globl	x___hsc2hs_sign___
	.p2align	3
x___hsc2hs_sign___:
	.long	0                       @ 0x0
	.long	0
	.size	x___hsc2hs_sign___, 8

	.type	x,%object               @ @x
	.data
	.globl	x
	.p2align	3
x:
	.long	1                       @ 0x1
	.long	0
	.size	x, 8

	.type	y___hsc2hs_sign___,%object @ @y___hsc2hs_sign___
	.bss
	.globl	y___hsc2hs_sign___
	.p2align	3
y___hsc2hs_sign___:
	.long	0                       @ 0x0
	.long	0
	.size	y___hsc2hs_sign___, 8

	.type	y,%object               @ @y
	.data
	.globl	y
	.p2align	3
y:
	.long	4294967295              @ 0xffffffff
	.long	0
	.size	y, 8

	.type	z___hsc2hs_sign___,%object @ @z___hsc2hs_sign___
	.globl	z___hsc2hs_sign___
	.p2align	3
z___hsc2hs_sign___:
	.long	1                       @ 0x1
	.long	0
	.size	z___hsc2hs_sign___, 8

	.type	z,%object               @ @z
	.globl	z
	.p2align	3
z:
	.long	4294967295              @ 0xffffffffffffffff
	.long	4294967295
	.size	z, 8

	.type	.L.str,%object          @ @.str
	.section	.rodata.str1.1,"aMS",%progbits,1
.L.str:
	.asciz	"Hello World\" 12345"
	.size	.L.str, 19

	.type	t,%object               @ @t
	.data
	.globl	t
	.p2align	2
t:
	.long	.L.str
	.size	t, 4


	.ident	"clang version 5.0.1 (tags/RELEASE_501/final)"
	.section	".note.GNU-stack","",%progbits
