# a stub makefile for BSD make.
# just redirect to GNU make, which is commonly available as "gmake".

TARGETS=all clean distclean

.for T in ${TARGETS}
${T}:
	@gmake ${T}
.endfor
