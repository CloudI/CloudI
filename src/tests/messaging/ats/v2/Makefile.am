#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(cloudi_prefix)/tests/messaging"
inst_PROGRAMS = messaging_ats2
CLEANFILES = main_dats.c
messaging_ats2_SOURCES = main_dats.c
messaging_ats2_CFLAGS = $(ATS2_CFLAGS) $(PTHREAD_CFLAGS)
messaging_ats2_LDADD = $(top_builddir)/api/ats/v2/libcloudi_ats2.la \
                       $(PTHREAD_LIBS) $(CXX_STDLIB_LIB)

%_dats.c: %.dats
	PATSHOME=$(PATSHOME) $(ATS2CC) $(ATS2CFLAGS) \
                         -IIATS $(top_srcdir)/api/ats/v2/ -ccats $<

