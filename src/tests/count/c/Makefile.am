#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(cloudi_prefix)/tests/count"
inst_PROGRAMS = count_c
count_c_SOURCES = main.c
count_c_CFLAGS = -fexceptions -I$(top_srcdir)/api/c/
count_c_LDADD = $(top_builddir)/api/c/libcloudi.la $(CXX_STDLIB_LIB)

